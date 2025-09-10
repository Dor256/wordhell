module WordService where

import Data.Text ( Text, lines, toLower, length )
import Data.Text.IO (readFile)
import Prelude hiding (readFile, lines, length)
import Servant ( Handler )
import qualified Data.Set as Set
import qualified Data.List as List
import System.Random (randomRIO)

readAnswers :: IO [Text]
readAnswers = do
    contents <- readFile "answers.txt"
    let answers = lines contents
    return $ List.map toLower $ List.filter fiveLetterWords answers
    where
      fiveLetterWords = (==5) . length

findRandomWord :: [Text] -> Handler Text
findRandomWord possibleWords = (possibleWords !!) <$> randomRIO (0, List.length possibleWords - 1)

readDictionary :: IO (Set.Set Text)
readDictionary = do
    contents <- readFile "words.txt"
    let dictionary = Set.fromList $ lines contents
    return $ Set.map toLower $ Set.filter fiveLetterWords dictionary
    where
        fiveLetterWords = (==5) . length
