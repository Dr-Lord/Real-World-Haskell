-- File: Real World Haskell/Ch24/Sorting.hs
module Sorting where

import Control.Parallel (par, pseq)
import Data.List (sort)

    -- Regular implementation
sort' :: (Ord a) => [a] -> [a]
sort' (x:xs) = lesser ++ x:greater
    where lesser  = sort' [y | y <- xs, y <  x]
          greater = sort' [y | y <- xs, y >= x]
sort' _ = []


    -- Good parallel implementation
parSort :: (Ord a) => [a] -> [a]
parSort (x:xs)    = force greater `par` (force lesser `pseq`
                                         (lesser ++ x:greater))
    where lesser  = parSort [y | y <- xs, y <  x]
          greater = parSort [y | y <- xs, y >= x]
parSort _         = []

    -- Function to force full evaluation of a list
force :: [a] -> ()
force xs = go xs `pseq` ()
    where go (_:xs) = go xs
          go [] = 1


    -- Bad parallel implementation (removed force function)
sillySort (x:xs) = greater `par` (lesser `pseq`
                                  (lesser ++ x:greater))
    where lesser   = sillySort [y | y <- xs, y <  x]
          greater  = sillySort [y | y <- xs, y >= x]
sillySort _        = []


    -- No force and preq instead of par
seqSort :: (Ord a) => [a] -> [a]
seqSort (x:xs) = lesser `pseq` (greater `pseq`
                                (lesser ++ x:greater))
    where lesser  = seqSort [y | y <- xs, y <  x]
          greater = seqSort [y | y <- xs, y >= x]
seqSort _ = []


    -- Since par is cheap but not free, change to non-parallel sort after some treshold
parSort2 :: (Ord a) => Int -> [a] -> [a]
parSort2 d list@(x:xs)
  | d <= 0    = sort list
  | otherwise = force greater `par` (force lesser `pseq`
                                     (lesser ++ x:greater))
      where lesser      = parSort2 d' [y | y <- xs, y <  x]
            greater     = parSort2 d' [y | y <- xs, y >= x]
            d' = d - 1
parSort2 _ _              = []


-- Exercise 1
    -- parsort2 with treshold based on sublist length
parSort3 :: (Ord a) => Int -> [a] -> [a]
parSort3 d list@(x:xs)
  | length list <= d = sort list
  | otherwise        = force greater `par` (force lesser `pseq`
                                     (lesser ++ x:greater))
      where lesser      = parSort2 d [y | y <- xs, y <  x]
            greater     = parSort2 d [y | y <- xs, y >= x]
parSort3 _ _              = []
