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

staticPath :: IO FilePath
staticPath = do 
    curDir <- getCurrentDirectory
    return $ curDir ++ "/../client/dist/"

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
findRandomWord possibleWords = do
    index <- randomRIO (0, List.length possibleWords - 1)
    return $ possibleWords !! index

main :: IO ()
main = do 
  dictionary <- readDictionary
  answers <- readAnswers
  statics <- staticPath

  scotty 3000 $ do

  middleware $ staticPolicy (addBase statics)
  
  get "/" $ file $ statics ++ "index.html"

  get "/words" $ json dictionary

  get "/word" $ do
    word <- findRandomWord answers
    text word
