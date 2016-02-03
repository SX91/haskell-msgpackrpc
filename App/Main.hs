{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import           BasicPrelude              hiding (catch, finally, try)

-- import           Control.Monad.Catch
import qualified Data.Map                  as Map

import           Network.MsgpackRpc.Server

import           Debug.Trace

default (Text)


testMethod :: Int -> RpcMethodT IO Text
testMethod = return . show

main :: IO ()
main = do
    traceIO "---->"
    let methodMap =  Map.fromList [("test", rpcMethod testMethod)]
    serveRpc 128 (serverSettings 5252 "*") methodMap

