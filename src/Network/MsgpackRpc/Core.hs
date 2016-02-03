{-|
Module      : Network.MsgpackRpc.Core
Description : MessagePack RPC core implementation for client and server
Copyright   : 2016 Anton Kondrachkov
License     : BSD3
Maintainer  : Anton Kondrachkov <SimpleX91@gmail.com>
Stability   : Stable
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Network.MsgpackRpc.Core
    (
      RpcT
    , Session
    , receiveMessage
    , sendMessage
    , sendResponse
    , sendError
    , isClosed
    , forkRpcT
    , execRpcT
    ) where

import           BasicPrelude                      hiding (catch, finally,
                                                    handle, try)

import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Lifted         (ThreadId, fork)
import           Control.Concurrent.STM
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Trans.Control

import qualified Data.ByteString.Lazy              as BL
import           Data.Conduit
import           Data.Conduit.Serialization.Binary
import           Data.Conduit.TMChan

import           Network.MsgpackRpc.Spec

import           Debug.Trace

default (Text)

--------------------------------------------------------------------------------
type RpcT = ReaderT Session

decodeConduit :: (Monad m, MonadThrow m, MessagePack o)
              => Conduit ByteString m o
decodeConduit = mapOutputMaybe fromObject conduitDecode
{-# INLINABLE decodeConduit #-}

encodeConduit :: (Monad m, MonadThrow m, MessagePack o)
              => Conduit o m ByteString
encodeConduit = awaitForever (yield . BL.toStrict . pack)
{-# INLINABLE encodeConduit #-}

data Session
    = Session
    { inChan  :: !(TBMChan Message)
    , outChan :: !(TBMChan Message)
    }

newtype ServerError = ServerError Text
  deriving (Show, Typeable)

instance Exception ServerError

-- | Create new RPC connection session.
newSession :: Int  -- ^ Size of bounded channel.
           -> STM Session  -- ^ RPC connection session.
newSession qSize =
    Session <$> newTBMChan qSize <*> newTBMChan qSize
{-# INLINE newSession #-}

-- | Close RPC connection session.
closeSession :: Session -> STM ()
closeSession Session{..} = do
    closeTBMChan inChan
    closeTBMChan outChan
{-# INLINE closeSession #-}

-- | Receive RPC message.
-- If connection is closed, returns Nothing.
receiveMessage :: (MonadIO m, MonadThrow m)
               => RpcT m (Maybe Message)
receiveMessage = do
    i <- reader inChan
    liftIO . atomically $! readTBMChan i
{-# INLINABLE receiveMessage #-}

-- | Send RPC message.
sendMessage :: (MonadIO m, MonadThrow m)
            => Message
            -> RpcT m ()
sendMessage msg = do
    out <- reader outChan
    liftIO . atomically . writeTBMChan out $ msg
{-# INLINABLE sendMessage #-}

-- | Send RPC response message.
-- Value is serialized internally.
sendResponse :: (MonadIO m, MonadThrow m, MessagePack o)
             => MessageId  -- ^ Message ID.
             -> o  -- ^ Response value.
             -> RpcT m ()
sendResponse msgid obj =
    sendMessage $! Response msgid Nothing (Just $! toObject obj)
{-# INLINABLE sendResponse #-}

-- | Send RPC error message.
--
-- To send RPC error use `rpcError`:
-- > sendError msgid $ rpcError ".MyServer.MyError" myErrorData
--
-- To send any exception use `internalError`:
-- > sendError msgid $ internalError (err :: SomeException)
sendError :: (MonadIO m, MonadThrow m)
             => MessageId  -- ^ Message ID.
             -> RpcError  -- ^ Response error.
             -> RpcT m ()
sendError msgid obj =
    sendMessage $! Response msgid (Just obj) Nothing
{-# INLINABLE sendError #-}

-- | Check if session is closed.
isClosed :: (MonadIO m)
         => RpcT m Bool
isClosed = reader outChan >>= (liftIO . atomically . isClosedTBMChan)
{-# INLINABLE isClosed #-}

-- | Fork new RPC thread.
forkRpcT :: (MonadBaseControl IO m, MonadIO m, MonadThrow m, MonadCatch m)
        => RpcT m a
        -> RpcT m ThreadId
forkRpcT f = ask >>= fork . void . lift . runReaderT f
{-# INLINE forkRpcT #-}

-- | Execute RpcT.
-- Can work with any Sink and Source.
-- Uses two separate threads to read from source and to write to sink.
execRpcT :: (MonadBaseControl IO m, MonadIO m, MonadThrow m, MonadCatch m)
        => Int  -- ^ Channel queue size.
        -> Sink ByteString m ()  -- ^ Sink to write to.
        -> Source m ByteString  -- ^ Source to read from.
        -> RpcT m a  -- ^ RpcT to run.
        -> m a
execRpcT qSize sink source f = do
    traceM "execRpcT begin"
    session@Session{..} <- liftIO . atomically $ newSession qSize
    let inSink = sinkTBMChan inChan True
        outSource = sourceTBMChan outChan
        sourcePipe = source =$= decodeConduit $$ inSink
        sinkPipe = outSource =$= encodeConduit $$ sink

    withAsync (sourcePipe `catch` handler session) $ const $
        withAsync (sinkPipe `catch` handler session) $ \o -> do
            res <- runReaderT f session
            liftIO $ do
                atomically $ closeSession session
                void $ wait o
            traceM "execRpcT end"
            return res
  where
    handler session err = do
        traceShowM (err :: SomeException)
        liftIO . atomically $ closeSession session
        throwM $! ServerError "forcibly closed"

