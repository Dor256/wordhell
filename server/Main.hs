{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Text.Lazy (Text, lines, length, toLower)
import qualified Data.List as List
import Prelude hiding (lines, readFile, length)
import Data.Text.Lazy.IO (readFile)
import qualified Data.Set as Set
import System.Random
import Network.Wai.Middleware.Static
import System.Directory
import qualified System.Environment as Env
import Data.Maybe (fromMaybe)

staticPath :: IO FilePath
staticPath = fmap (++ "/../client/static/") getCurrentDirectory

readAnswers :: IO [Text]
readAnswers = do
    contents <- readFile "answers.txt"
    let answers = lines contents
    return $ List.map toLower $ List.filter fiveLetterWords answers
    where
        fiveLetterWords = (==5) . length

readDictionary :: IO (Set.Set Text)
readDictionary = do
    contents <- readFile "words.txt"
    let dictionary = Set.fromList $ lines contents
    return $ Set.map toLower $ Set.filter fiveLetterWords dictionary
    where
        fiveLetterWords = (==5) . length


findRandomWord :: [Text] -> ActionM Text
findRandomWord possibleWords = (possibleWords !!) <$> randomRIO (0, List.length possibleWords - 1)

getEnv :: IO String
getEnv = fromMaybe "development" <$> Env.lookupEnv "APP_ENV"

main :: IO ()
main = do
  dictionary <- readDictionary
  answers <- readAnswers
  statics <- staticPath
  env <- getEnv
  scotty 3000 $ do

    middleware $ staticPolicy (addBase statics)

    get "/" $ file $ statics ++ "index.html"

    get "/words" $ json dictionary

    get "/word" $ findRandomWord answers >>= text
