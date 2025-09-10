{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

import Prelude hiding (log)
import qualified System.Environment as Env
import Servant
import Network.Wai.Handler.Warp (setPort, setBeforeMainLoop, defaultSettings, runSettings)
import WordRouter ( WordRouter, wordRouter )
import StaticRouter ( StaticApi, serveClient )
import Middleware (cors, log)

type Api = WordRouter
  :<|> StaticApi

api :: Proxy Api
api = Proxy

router :: Server Api
router = wordRouter
  :<|> serveClient

server :: Application
server = serve api router

main :: IO ()
main =
  let
    port = 3000
    settings = setPort port $ setBeforeMainLoop (putStrLn $ "Server started on port " ++ show port ++ "\n") defaultSettings
    middleware = log . cors
  in
    runSettings settings $ middleware server
