{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Text (Text, lines, length, toLower)
import qualified Data.List as List
import Prelude hiding (lines, readFile, length)
import Data.Text.IO (readFile)
import System.Random
import Network.Wai.Middleware.Static

staticPath :: String
staticPath = "/Users/dorb/Workspace/wordhell/client/dist/"

readDictionary :: IO [Text]
readDictionary = do
    contents <- readFile "words.txt"
    let dictionary = lines contents
    return $ List.map toLower $ List.filter ((==5) . length) dictionary

findRandomWord :: [Text] -> ActionM Text
findRandomWord possibleWords = do
    index <- randomRIO (0, List.length possibleWords - 1)
    return $ possibleWords !! index

main :: IO ()
main = do 
  dictionary <- readDictionary
  scotty 3000 $ do

  middleware $ staticPolicy $ addBase staticPath
  
  get "/" $ file $ staticPath ++ "index.html"

  get "/words" $ json dictionary

  get "/word" $ do
    word <- findRandomWord dictionary
    json word
