[![Build Status](https://travis-ci.org/SimpleX91/haskell-msgpackrpc.svg?branch=master)](https://travis-ci.org/SimpleX91/haskell-msgpackrpc)
# haskell-msgpackrpc
MsgPack RPC implementation for Haskell (server side only, for now)

## Example
```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import           BasicPrelude              hiding (catch, finally, try)

import           Control.Monad.Catch
import qualified Data.Map                  as Map

import           Network.MsgpackRpc.Server


default (Text)

-- This sums arguments
sumMethod :: Int -> Int -> RpcMethodT IO Int
sumMethod a b = return $! a + b

-- This throws error to client
errorMethod :: RpcMethodT IO Int
errorMethod = throwM $ rpcError ".MyRpc.SomeCoolError" ("WAT!?" :: Text)

func :: IO ()
func = do
    let methodMap =  Map.fromList
            [
              ("sum", rpcMethod sumMethod)
            , ("error", rpcMethod errorMethod)
            ]
    serveRpc 128 (serverSettings 5252 "*") methodMap

main :: IO ()
main = func

```
