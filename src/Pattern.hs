module Pattern where

import Data.List

import Data.Text (Text)

type Pattern a = [a]

candidates :: [[Text]] -> [Pattern Text] -> [Pattern Text]
candidates t xs = concat $ map (\l -> candidates' l $ concat $ filter (isInfixOf l) t) xs

candidates' :: Pattern Text -> [Text] -> [Pattern Text]
candidates' _ [] = []
candidates' p all@(x:xs) = case p `isPrefixOf` all of
  True -> [take (genericLength p + 1) all] ++ candidates' p xs
  False -> candidates' p xs

instanceOf :: (Eq a, Ord a) => Pattern a -> [a] -> Bool
instanceOf _ [] = False
instanceOf p xs = p `isPrefixOf` xs || instanceOf p (tail xs)


