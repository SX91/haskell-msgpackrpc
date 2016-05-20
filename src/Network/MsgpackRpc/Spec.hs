{-|
Module      : Network.MsgpackRpc.Spec
Description : MessagePack RPC spec implementation
Copyright   : 2016 Anton Kondrachkov
License     : BSD3
Maintainer  : Anton Kondrachkov <SimpleX91@gmail.com>
Stability   : Stable
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.MsgpackRpc.Spec
    (
      Message(..)
    , MessageID(..)
    , MessageType(..)

    , RpcError(..)

    , module Data.MessagePack
    ) where

import           BasicPrelude

import           Data.MessagePack
import qualified Data.Vector             as V

import           Network.MsgpackRpc.Util

data RpcError
    = RpcError { errorCode :: !Text, errorBody :: !Object }
  deriving (Eq, Ord, Show, Typeable)

instance Exception RpcError

instance MessagePack RpcError where
    toObject (RpcError code body) = toObject (code, body)
    {-# INLINE toObject #-}
    fromObject o = uncurry RpcError <$> fromObject o
    {-# INLINE fromObject #-}

--------------------------------------------------------------------------------
data MessageType = RequestMessage
                 | ResponseMessage
                 | NotifyMessage
  deriving (Show, Eq, Ord, Bounded, Enum)

instance MessagePack MessageType where
    toObject RequestMessage = toObject (0 :: Int)
    toObject ResponseMessage = toObject (1 :: Int)
    toObject NotifyMessage = toObject (2 :: Int)
    {-# INLINE toObject #-}

    fromObject o = toEnumSafe =<< fromObject o
    {-# INLINE fromObject #-}

newtype MessageID = MessageID Word32
  deriving (Show, Eq, Bounded, Ord)

instance MessagePack MessageID where
    toObject (MessageID i) = toObject i
    {-# INLINE toObject #-}

    fromObject o = MessageID <$> fromObject o
    {-# INLINE fromObject #-}

type Method = Text
type Args = [Object]
type Result = Either RpcError Object

data Message = Request !MessageID !Method Args
             | Response !MessageID Result
             | Notify !Method Args
  deriving (Show, Eq, Ord)

instance MessagePack Message where
    toObject (Request msgid method args) = toObject (RequestMessage, msgid, method, args)
    toObject (Response msgid result) =
        case result of
            Left failure -> toObject (ResponseMessage, msgid, failure, ())
            Right value -> toObject (ResponseMessage, msgid, (), value)
    toObject (Notify method args) = toObject (NotifyMessage, method, args)
    {-# INLINE toObject #-}

    fromObject (ObjectArray arr) =
        let size = V.length arr
            mtype = fromObject (V.head arr)
            xs = V.tail arr
         in case (size, mtype) of
            (4, Just RequestMessage) -> toRequest xs
            (4, Just ResponseMessage) -> toResponse xs
            (3, Just NotifyMessage) -> toNotify xs
            _ -> Nothing
      where
        toRequest :: Vector Object -> Maybe Message
        toRequest [msgId, msgMethod, msgArgs] =
            Request <$> fromObject msgId <*> fromObject msgMethod <*> fromObject msgArgs
        toRequest _ = Nothing

        toResponse :: Vector Object -> Maybe Message
        toResponse [msgId, msgError, msgResult] =
            Response <$> fromObject msgId <*> (choose <$> fromObject msgError <*> fromObject msgResult)
          where
            choose (Just e) _ = Left (e :: RpcError)
            choose _ o = Right o
        toResponse _ = Nothing

        toNotify :: Vector Object -> Maybe Message
        toNotify [msgMethod, msgArgs] =
            Notify <$> fromObject msgMethod <*> fromObject msgArgs
        toNotify _ = Nothing
    fromObject _ = Nothing
    {-# INLINE fromObject #-}

