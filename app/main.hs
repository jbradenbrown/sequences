module Main where

import Pattern
import Apriori

import Data.Text (Text)
import qualified Data.Text as T

import Data.List (intersperse, sortOn)

import qualified Data.Text.IO as T (hGetContents, putStr)
import System.IO (stdin)
import System.Environment (getArgs)

-- Process ; separated input into lines
processFile :: Text -> [[Text]]
processFile = map (T.words) . T.lines

outputFormat :: (Show b, Ord b) => [(Pattern Text, b)] -> Text
outputFormat = T.unlines . (map format) . reverse . (sortOn snd)
               where format pf = T.append (T.pack ((show . snd $ pf) ++ ":")) $ (T.concat . semicolons . fst) pf
                     semicolons = (intersperse $ T.singleton ';')

main :: IO ()
main = do
  support <- fmap (read . (!! 0)) getArgs :: IO Double
  processedFile <- fmap processFile $ T.hGetContents stdin
  T.putStr $ outputFormat $ apriori support processedFile
