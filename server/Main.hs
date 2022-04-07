{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Text.Lazy (Text, lines, length, toLower)
import qualified Data.List as List
import Prelude hiding (lines, readFile, length)
import Data.Text.Lazy.IO (readFile)
import qualified Data.Set as Set
import System.Random
import Network.Wai.Middleware.Static

staticPath :: String
staticPath = "/Users/dorb/Workspace/wordhell/client/dist/"

readAnswers :: IO [Text]
readAnswers = do
    contents <- readFile "answers.txt"
    let answers = lines contents
    return $ List.map toLower $ List.filter ((==5) . length) answers

readDictionary :: IO (Set.Set Text)
readDictionary = do
    contents <- readFile "words.txt"
    let dictionary = Set.fromList $ lines contents
    return $ Set.map toLower $ Set.filter((==5) . length) dictionary
    

findRandomWord :: [Text] -> ActionM Text
findRandomWord possibleWords = do
    index <- randomRIO (0, List.length possibleWords - 1)
    return $ possibleWords !! index

main :: IO ()
main = do 
  dictionary <- readDictionary
  answers <- readAnswers

  scotty 3000 $ do

  middleware $ staticPolicy $ addBase staticPath
  
  get "/" $ file $ staticPath ++ "index.html"

  get "/words" $ json dictionary

  get "/word" $ do
    word <- findRandomWord answers
    text word
