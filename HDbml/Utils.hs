-- 10/19/22
-- Utils.hs
module HDbml.Utils where

-- Used to add a value to a list only if it doesn't already exist
(?:) :: (Eq a) => a -> [a] -> [a]
(?:) x xs | x `elem` xs = xs
          | otherwise = (x:xs)
infixr 5 ?:
