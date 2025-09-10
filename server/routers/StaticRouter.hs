module StaticRouter where

import Servant

type StaticApi = Raw

serveClient :: Server StaticApi
serveClient = serveDirectoryFileServer "../client/static"
