-- File: Real World Haskell\Ch8\GlobRegex.hs
module GlobRegex
    (
      globToRegex
    , matchesGlob
    , matchesGlobSensitive
    ) where

import Text.Regex.Posix ((=~))
import Data.Char

matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = name =~ globToRegex pat

globToRegex :: String -> String
globToRegex cs = '^' : globToRegex' cs ++ "$"
    where globToRegex' :: String -> String
          globToRegex' ""             = ""
          globToRegex' ('*':cs)       = ".*" ++ globToRegex' cs
          globToRegex' ('?':cs)       = '.' : globToRegex' cs
          globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
          globToRegex' ('[':c:cs)     = '['  :  c : charClass cs
          globToRegex' ('[':_)        = error "unterminated character class"
          globToRegex' (c:cs)         = escape c ++ globToRegex' cs
          
          escape :: Char -> String
          escape c | c `elem` regexChars = '\\' : [c]
                   | otherwise = [c]
              where regexChars = "\\+()^$.{}]|"
              
          charClass :: String -> String
          charClass (']':cs) = ']' : globToRegex' cs
          charClass (c:cs)   = c : charClass cs
          charClass []       = error "unterminated character class"
          
-- 1
-- Laziness allows the function to return the full translation with an error at the end
-- if the glob is a malformed pattern
globToUpperRegex :: String -> String
globToUpperRegex = map toUpper . globToRegex

-- 2
matchesGlobSensitive :: FilePath -> String -> Bool -> Bool
matchesGlobSensitive name pat sensitive = name =~ globToRegexSensitive pat sensitive

globToRegexSensitive :: String -> Bool -> String
globToRegexSensitive cs sensitive = '^' : globToRegexSensitive' cs sensitive ++ "$"
    where globToRegexSensitive' :: String -> Bool -> String
          globToRegexSensitive' ""             s = ""
          globToRegexSensitive' ('*':cs)       s = ".*" ++ globToRegexSensitive' cs s
          globToRegexSensitive' ('?':cs)       s = '.' : globToRegexSensitive' cs s
          globToRegexSensitive' ('[':'!':c:cs) s | s = "[^" ++ (toUpper c) : charClass cs s
                                                 | otherwise = "[^" ++ c : charClass cs s
          globToRegexSensitive' ('[':c:cs)     s | s = '[' : (toUpper c) : charClass cs s
                                                 | otherwise = '[' : c : charClass cs s
          globToRegexSensitive' ('[':_)        s = error "unterminated character class"
          globToRegexSensitive' (c:cs)         s | s = escape (toUpper c) ++ globToRegexSensitive' cs s
                                                 | otherwise = escape c ++ globToRegexSensitive' cs s
          
          escape :: Char -> String
          escape c | c `elem` regexChars = '\\' : [c]
                   | otherwise = [c]
              where regexChars = "\\+()^$.{}]|"
              
          charClass :: String -> Bool -> String
          charClass (']':cs) s = ']' : globToRegexSensitive' cs s
          charClass (c:cs)   s | s = (toUpper c) : charClass cs s
                               | otherwise = c : charClass cs s
          charClass []       s = error "unterminated character class"

    -- OR
globToRegexSensitive2 :: String -> Bool -> String    
globToRegexSensitive2 cs sensitive | sensitive = globToRegex (map toUpper cs)
                                   | otherwise = globToRegex cs
