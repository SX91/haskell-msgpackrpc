{-|
Module      : Network.MsgpackRpc.Util
Description : Some stuff
Copyright   : 2016 Anton Kondrachkov
License     : BSD3
Maintainer  : Anton Kondrachkov <SX91@protonmail.com>
Stability   : Stable
-}

{-# LANGUAGE BangPatterns #-}

module Network.MsgpackRpc.Util
    (
      toEnumSafe
    ) where

toEnumSafe :: (Enum a, Bounded a) => Int -> Maybe a
toEnumSafe !i
    | minBound <= i && i <= maxBound = Just $! toEnum i
    | otherwise = Nothing
{-# INLINE toEnumSafe #-}

