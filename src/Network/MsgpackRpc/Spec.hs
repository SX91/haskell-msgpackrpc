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

newtype RpcError
    = RpcError { errorBody :: Object }
  deriving (Eq, Ord, Show, Typeable)

instance Exception RpcError

instance MessagePack RpcError where
    toObject (RpcError o) = o
    {-# INLINE toObject #-}
    fromObject o = Just (RpcError o)
    {-# INLINE fromObject #-}

--------------------------------------------------------------------------------
data MessageType = RequestMessage
                 | ResponseMessage
                 | NotifyMessage
  deriving (Show, Eq, Ord, Bounded, Enum)

instance MessagePack MessageType where
    toObject RequestMessage = ObjectUInt 0
    toObject ResponseMessage = ObjectUInt 1
    toObject NotifyMessage = ObjectUInt 2
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

data Message = Request !MessageID !Text [Object]
             | Response !MessageID !(Maybe RpcError) !(Maybe Object)
             | Notify !Text [Object]
  deriving (Show, Eq, Ord)

instance MessagePack Message where
    toObject (Request msgid method args) = toObject (RequestMessage, msgid, method, args)
    toObject (Response msgid failure result) = toObject (ResponseMessage, msgid, failure, result)
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
            Response <$> fromObject msgId <*> fromObject msgError <*> fromObject msgResult
        toResponse _ = Nothing

        toNotify :: Vector Object -> Maybe Message
        toNotify [msgMethod, msgArgs] =
            Notify <$> fromObject msgMethod <*> fromObject msgArgs
        toNotify _ = Nothing
    fromObject _ = Nothing
    {-# INLINE fromObject #-}

