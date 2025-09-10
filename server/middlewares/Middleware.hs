{-# LANGUAGE OverloadedStrings #-}

module Middleware where

import Network.Wai (Middleware, requestMethod, rawPathInfo, modifyResponse, mapResponseHeaders)

cors :: Middleware
cors application req res = do
    let
      origins =
        [ ("Access-Control-Allow-Origin", "*")
        , ("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
        , ("Access-Control-Allow-Headers", "Content-Type, Authorization")
        , ("Access-Control-Allow-Credentials", "true")
        ]
    modifyResponse (mapResponseHeaders (origins ++)) application req res


log :: Middleware
log application req res = do
    putStrLn $ show (requestMethod req) ++ " "  ++ show (rawPathInfo req)
    application req res
