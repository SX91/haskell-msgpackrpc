{-|
Module      : Network.MsgpackRpc.Server
Description : MessagePack RPC Server implementation
Copyright   : 2016 Anton Kondrachkov
License     : BSD3
Maintainer  : Anton Kondrachkov <SimpleX91@gmail.com>
Stability   : Experimental
-}

{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}


module Network.MsgpackRpc.Server
    (
      RpcError(..)
    , module Network.MsgpackRpc.Exception

    , serveRpc
    , runRpcServer

    , rpcMethod
    , RpcMethod
    , RpcMethodT
    , RpcMethodMap

    , ServerSettings
    , serverSettings
    ) where

import           BasicPrelude                 hiding (catch, finally, try)

import           Control.Monad.Catch
import           Control.Monad.Trans.Control
import           Data.Conduit.Network
import qualified Data.HashMap.Strict          as HashMap

import           Network.MsgpackRpc.Core
import           Network.MsgpackRpc.Exception
import           Network.MsgpackRpc.Method
import           Network.MsgpackRpc.Spec

import           Debug.Trace


default (Text)

type RpcMethodMap m = HashMap Text (RpcMethod m)

serveClient :: (MonadBaseControl IO m, MonadThrow m, MonadCatch m, MonadIO m)
            => RpcMethodMap m
            -> RpcT m ()
serveClient methodMap = receiveForever processMessage
  where
    processMessage (Request msgid method args)  =
        void . forkRpcT $ handleRequest msgid method args
    processMessage (Notify method args) =
        void . forkRpcT $ handleNotify method args
    processMessage Response{} = return ()

    execMethod method args =
        let throwNoMethod = throwM $ noMethodError method
            execute f = lift $! methodBody f args
         in maybe throwNoMethod execute (HashMap.lookup method methodMap)

    handleRequest msgid method args =
        fmap Right (execMethod method args) `catches` handlers
        >>= either (sendError msgid) (sendResponse msgid)
      where
        handlers = [
              Handler (\(err :: RpcError) -> return (Left err))
            , Handler (\(err :: SomeException) -> return . Left $ internalError err)
            ]

    handleNotify method args =
        void (execMethod method args) `catch` handler
      where
        handler (err :: SomeException) = traceShowM err

runRpcServer :: (MonadBaseControl IO m, MonadIO m, MonadThrow m, MonadCatch m)
              => Int
              -> ServerSettings
              -> RpcT m ()
              -> m ()
runRpcServer qSize ss f =
    runGeneralTCPServer ss $ \ad ->
        execRpcT qSize (appSink ad) (appSource ad) f

serveRpc :: (MonadBaseControl IO m, MonadIO m, MonadThrow m, MonadCatch m)
         => Int
         -> ServerSettings
         -> RpcMethodMap m
         -> m ()
serveRpc qsize ss methods =
    runRpcServer qsize ss (serveClient methods)

