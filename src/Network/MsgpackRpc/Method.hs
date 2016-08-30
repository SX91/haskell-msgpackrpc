{-|
Module      : Network.MsgpackRpc.Method
Description : RPC Method wrappers.
Copyright   : 2016 Anton Kondrachkov
License     : BSD3
Maintainer  : Anton Kondrachkov <SX91@protonmail.com>
Stability   : Experimental
-}

{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.MsgpackRpc.Method
    (
      RpcMethod(..)
    , RpcMethodT(..)
    , RpcMethodType(..)
    , rpcMethod
    ) where

import           BasicPrelude
import           Control.Monad.Catch
import           Control.Monad.Trans

import           Network.MsgpackRpc.Exception
import           Network.MsgpackRpc.Spec

newtype RpcMethod m
    = RpcMethod { methodBody :: [Object] -> m Object }

newtype RpcMethodT m a
    = RpcMethodT { runMethod :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance MonadTrans RpcMethodT where
    lift = RpcMethodT
    {-# INLINE lift #-}

class (Monad m, MonadThrow m) => RpcMethodType m f where
    toBody :: f -> [Object] -> m Object

instance (Functor m, MonadThrow m, MessagePack o) => RpcMethodType m (RpcMethodT m o) where
    toBody f [] = toObject <$> runMethod f
    toBody _ _ = throwM $ methodError "invalid number of arguments"
    {-# INLINE toBody #-}

instance (MonadThrow m, MessagePack o, RpcMethodType m r) => RpcMethodType m (o -> r) where
    toBody f (x:xs) =
        case fromObject x of
            Nothing -> throwM $ methodError "argument type error"
            Just o -> toBody (f o) xs
    toBody _ _ =
        throwM $ methodError "not enough arguments"
    {-# INLINE toBody #-}

rpcMethod :: RpcMethodType m f => f -> RpcMethod m
rpcMethod = RpcMethod . toBody
{-# INLINABLE rpcMethod #-}

