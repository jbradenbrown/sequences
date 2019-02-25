module Apriori where

import Pattern
import Data.Text (Text)
import Data.Set (fromList)
import Data.List (nub, group, sort, genericLength, foldl', isInfixOf)

apriori :: (Num b, Ord b) => b -> [[Text]] -> [(Pattern Text, b)]
apriori s t = (apriori' s t) . frequent' $ singletons t
              where frequent' = frequent (genericLength t) s

apriori' :: (Num b, Ord b) => b -> [[Text]] -> [(Pattern Text, b)] -> [(Pattern Text, b)]
apriori' _ _ [] = []
apriori' s t ps = ps ++ (apriori' s t $ frequent' $ candidates t $ map fst $ ps)
  where frequent' xs = (frequent (genericLength t) s) $ foldl' (\acc n -> acc ++ filter (`isInfixOf` n) xs) [] t

count :: (Eq a, Ord a, Num b) => [Pattern a] -> [(Pattern a, b)]
count = map (\x -> (head x, genericLength x)) . group . sort

singletons :: (Eq a, Ord a) => [[a]] -> [Pattern a]
singletons = (map (:[])) . concat

frequent :: (Eq a, Ord a, Num b, Ord b) => b -> b -> [Pattern a] -> [(Pattern a, b)]
frequent ls s = filter ((> ls * s) . snd) . count
