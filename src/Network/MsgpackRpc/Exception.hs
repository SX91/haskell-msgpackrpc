{-|
Module      : Network.MsgpackRpc.Error
Description : Various RPC errors
Copyright   : 2016 Anton Kondrachkov
License     : BSD3
Maintainer  : Anton Kondrachkov <SX91@protonmail.com>
Stability   : Experimental
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.MsgpackRpc.Exception
    (
      RpcError(..)
    , rpcError
    , internalError
    , methodError
    , noMethodError
    ) where

import           BasicPrelude

import           Network.MsgpackRpc.Spec

rpcError :: (MessagePack o)
         => Text
         -> o
         -> RpcError
rpcError errCode = RpcError errCode . toObject
{-# INLINE rpcError #-}

internalError :: (Exception e) => e -> RpcError
internalError = rpcError ".InternalError" . show
{-# INLINE internalError #-}

methodError :: Text -> RpcError
methodError = rpcError ".CallError"
{-# INLINE methodError #-}

noMethodError :: Text -> RpcError
noMethodError = rpcError ".CallError.NoMethodError"
{-# INLINE noMethodError #-}

