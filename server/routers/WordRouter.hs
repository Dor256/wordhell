{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module WordRouter where

import Data.Text (Text)
import Servant
import qualified Data.Set as Set
import Control.Monad.IO.Class
import WordService

type WordRoute = "word" :> Get '[PlainText] Text

getWord :: Server WordRoute
getWord = liftIO readAnswers >>= findRandomWord

type WordsRoute = "words" :> Get '[JSON] [Text]

getWords :: Server WordsRoute
getWords = liftIO (Set.toList <$> readDictionary)

type WordRouter = WordsRoute :<|> WordRoute

wordRouter :: Server WordRouter
wordRouter = getWords :<|> getWord
