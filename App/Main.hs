{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import           BasicPrelude              hiding (catch, finally, try)

import           Control.Monad.Catch
import qualified Data.Map                  as Map

import           Network.MsgpackRpc.Server


default (Text)


sumMethod :: Int -> Int -> RpcMethodT IO Int
sumMethod a b = return $! a + b

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

