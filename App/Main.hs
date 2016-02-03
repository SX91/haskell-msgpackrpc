{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import           BasicPrelude              hiding (catch, finally, try)

-- import           Control.Monad.Catch
import qualified Data.Map                  as Map

import           Network.MsgpackRpc.Server

import           Debug.Trace

default (Text)


testMethod :: Int -> Int -> RpcMethodT IO Int
testMethod a b = return $! a + b

main :: IO ()
main = do
    traceIO "---->"
    let methodMap =  Map.fromList [("sum", rpcMethod testMethod)]
    serveRpc 128 (serverSettings 5252 "*") methodMap

