module Pattern where

import Data.List

import Data.Text (Text)

type Pattern a = [a]

candidates :: [Pattern Text] -> [Pattern Text]
candidates xs = [(head a):b | a <- xs,
                              b <- xs,
                              (tail a) `isPrefixOf` b]

instanceOf :: (Eq a, Ord a) => Pattern a -> [a] -> Bool
instanceOf _ [] = False
instanceOf p xs = p `isPrefixOf` xs || instanceOf p (tail xs)


