-- File: Real World Haskell\Ch4.hs
import Data.List
import Data.Char (digitToInt, isSpace)
import Data.Foldable (foldlM)

import System.Environment (getArgs)
interactWith steption inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (steption input)

main = mainWith ex4 -- Change to ex4 for 4th exercise
  where mainWith steption = do
          args <- getArgs
          case args of
            [input,output] -> interactWith steption input output
            _ -> putStrLn "error: exactly two arguments needed"

-- 1
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (x:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit list = Just (unsafeInit list)
                where unsafeInit (x:[]) = []
                      unsafeInit (x:xs) = [x] ++ unsafeInit xs
                      
-- 2
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith cond list = let (piece, rest) = break cond list in
                        case rest of
                            [] -> []
                            _  -> [piece] ++ splitWith cond (tail rest)
                            
-- 3
ex3 = unlines . firstWords . lines
    where firstWords [] = []
          firstWords (x:xs) = (realHead (words x)) : (firstWords xs) -- Empty lines allowed
            where realHead [] = []
                  realHead x  = head x
           
-- 4
ex4 = unlines . transpose'b . lines -- change between transpose' and transpose'b

transpose' txt = if any null txt then [] -- No empty lines allowed
                    else (concat firsts) : (transpose' rests)
                        where (firsts, rests) = unzip $ map (splitAt 1) txt
    -- OR
transpose'b txt
    | null txt = []
    | any null txt = []
    | otherwise = (map head txt) : (transpose'b (map tail txt))

-- B --
   
-- 1
asInt_fold :: String -> Int
asInt_fold "" = error "No number provided"
asInt_fold xs = foldl step 0 xs
    where step acc x
            | not (x `elem` "0123456789abcdef") = error (show x ++ " is not a digit")
            | otherwise = acc * 10 + digitToInt x
   
-- 2
type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either "" = Left "No number provided"
asInt_either xs
    | any (\x -> not (x `elem` "0123456789abcdef")) xs = Left ("Non digit character(s) present")
    | otherwise = Right (foldl (\acc x -> acc * 10 + digitToInt x) 0 xs)

    -- OR
asInt_either' :: String -> Either ErrorMessage Int
asInt_either' "" = Left "No number provided"
asInt_either' xs = foldlM step 0 xs
    where step acc x
            | not (x `elem` "0123456789abcdef") = Left (show x ++ " is not a digit")
            | otherwise = Right (acc * 10 + digitToInt x)
        
-- 3
concat' ::[[a]] -> [a]
concat' = foldr (\xs acc -> xs ++ acc) []

-- 4
takeWhile_rec :: (a -> Bool) -> [a] -> [a]
takeWhile_rec _ [] = []
takeWhile_rec f (x:xs)
    | not (f x) = []
    | otherwise = x : (takeWhile_rec f xs)
    
takeWhile_foldr :: (a -> Bool) -> [a] -> [a]
takeWhile_foldr f = foldr step []
    where step x acc
            | not (f x) = []
            | otherwise = x:acc
       
-- 5
-- Weird exercise: groupBy expects an equivalence as an input function (reflexive, symmetric and transitive), which
-- leads to weird behaviuour when given an only transitive input function (<), since it just picks an element and 
-- "spans" to form a group.

-- This is a transitive-only-function-required version of groupBy, which operates on all pairs of consecutive values
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f = foldr step []
    where step x [] = [x]:[]
          step x ((z:zs):ys)
            | f x z = (x:z:zs):ys
            | otherwise = [x]:(z:zs):ys

-- 6
any' :: (a -> Bool) -> [a] -> Bool
any' f = foldl step False
    where step acc x = (f x) || acc
    
cycle' :: [a] -> [a]
cycle' xs = foldr step xs [1..]
    where step _ acc = xs ++ acc
    
words' :: String -> [String]
words' = dropWhile null . foldr step []
    where step x []
            | isSpace x = []
            | otherwise = [x]:[]
          step x (y:ys)
            | isSpace x = []:y:ys
            | otherwise = (x:y):ys
         
unwords' :: [String] -> String
unwords' = init . foldr (\x acc -> x ++ ' ':acc) ""
     