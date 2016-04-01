{-|
Module      : Network.MsgpackRpc.Error
Description : Various RPC errors
Copyright   : 2016 Anton Kondrachkov
License     : BSD3
Maintainer  : Anton Kondrachkov <SimpleX91@gmail.com>
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
rpcError errType errBody = RpcError $! toObject (errType, errBody)
{-# INLINE rpcError #-}

internalError :: (Exception e) => e -> RpcError
internalError = rpcError ".InternalError" . show
{-# INLINE internalError #-}

methodError :: Text -> RpcError
methodError = rpcError ".MethodError"
{-# INLINE methodError #-}

noMethodError :: Text -> RpcError
noMethodError = rpcError ".MethodError.NoMethodError"
{-# INLINE noMethodError #-}

