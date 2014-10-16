-- File: Real World Haskell\Ch8\Glob.hs
module Glob (namesMatching) where

import System.Info -- Necessary in Ex 1
import Data.List -- Necessary in Ex 3
import Text.Regex.Posix ((=~)) -- Necessary in Ex B1

import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))
import Control.Exception (handle)
import Control.Monad (forM)
import GlobRegex

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching :: String -> IO [String]
namesMatching pat
  | not (isPattern pat) = do
    exists <- doesNameExist pat
    return (if exists then [pat] else [])
  | otherwise = do
    case splitFileName pat of
      ("", baseName) -> do
          curDir <- getCurrentDirectory
          listMatches curDir baseName
      (dirName, baseName) -> do
          dirs <- if isPattern dirName
                  then namesMatching (dropTrailingPathSeparator dirName)
                  else return [dirName]
          let listDir = if isPattern baseName
                        then listMatches
                        else listPlain
          pathNames <- forM dirs $ \dir -> do
                           baseNames <- listDir dir baseName
                           return (map (dir </>) baseNames)
          return (concat pathNames)
          
doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
    fileExists <- doesFileExist name
    if fileExists
      then return True
      else doesDirectoryExist name
      
listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
    dirName' <- if null dirName
                then getCurrentDirectory
                else return dirName
    handle (const (return []):: IOError -> IO [String]) $ do
        names <- getDirectoryContents dirName'
        let names' = if isHidden pat
                     then filter isHidden names
                     else filter (not . isHidden) names
        return (filter (`matchesGlob` pat) names')

isHidden ('.':_) = True
isHidden _       = False

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
    exists <- if null baseName
              then doesDirectoryExist dirName
              else doesNameExist (dirName </> baseName)
    return (if exists then [baseName] else [])

    
-- 1
osSpecificMatchesGlob
    | onWindows = (\name pat -> matchesGlobSensitive name pat False)
    | otherwise = (\name pat -> matchesGlobSensitive name pat True)
        where onWindows = os == "mingw32"
        
osSpecificlistMatches :: FilePath -> String -> IO [String]
osSpecificlistMatches dirName pat = do
    dirName' <- if null dirName
                then getCurrentDirectory
                else return dirName
    handle (const (return []):: IOError -> IO [String]) $ do
        names <- getDirectoryContents dirName'
        let names' = if isHidden pat
                     then filter isHidden names
                     else filter (not . isHidden) names
        return (filter (`osSpecificMatchesGlob` pat) names') -- ONLY DIFFERENT LINE  

osSpecificNamesMatching :: String -> IO [String]
osSpecificNamesMatching pat
  | not (isPattern pat) = do
    exists <- doesNameExist pat
    return (if exists then [pat] else [])
  | otherwise = do
    case splitFileName pat of
      ("", baseName) -> do
          curDir <- getCurrentDirectory
          osSpecificlistMatches curDir baseName -- DIFFERENT LINE
      (dirName, baseName) -> do
          dirs <- if isPattern dirName
                  then osSpecificNamesMatching (dropTrailingPathSeparator dirName)
                  else return [dirName]
          let listDir = if isPattern baseName
                        then osSpecificlistMatches -- DIFFERENT LINE
                        else listPlain
          pathNames <- forM dirs $ \dir -> do
                           baseNames <- listDir dir baseName
                           return (map (dir </>) baseNames)
          return (concat pathNames)

-- 2 Not available

-- 3
isDeep :: String -> Bool
isDeep p = "**" `isInfixOf` p

newPats :: String -> (String, String)
newPats = foldr step ([], [])
    where step :: Char -> (String, String) -> (String, String)
          step '*' (d:ds, s:ss)
                     | d /= '*'  = ('*':d:ds, '*':s:ss)
                     | otherwise = ('*':'/':d:ds, s:ss)
          step c (deep, shallow) = (c:deep, c:shallow)

deepNamesMatching :: String -> IO [String]
deepNamesMatching pat
  | not (isPattern pat) = do
    exists <- doesNameExist pat
    return (if exists then [pat] else [])
  | otherwise = do
    if isDeep pat
        then do
            let (deepPat, shallowPat) = newPats pat
            shallowNames <- deepNamesMatching shallowPat
            deepNames <- deepNamesMatching deepPat
            return (shallowNames ++ deepNames)
        else do
            case splitFileName pat of
              ("", baseName) -> do
                  curDir <- getCurrentDirectory
                  listMatches curDir baseName
              (dirName, baseName) -> do
                  dirs <- if isPattern dirName
                          then deepNamesMatching (dropTrailingPathSeparator dirName)
                          else return [dirName]
                  let listDir = if isPattern baseName
                                then listMatches
                                else listPlain
                  pathNames <- forM dirs $ \dir -> do
                                   baseNames <- listDir dir baseName
                                   return (map (dir </>) baseNames)
                  return (concat pathNames)

-- B --

-- 1
type GlobError = String

safeGlobToRegex :: String -> Either GlobError String
safeGlobToRegex cs = '^' `prepend` (safeGlobToRegex' cs) `append` '$'
    where safeGlobToRegex' :: String -> Either GlobError String
          safeGlobToRegex' ""             = Right ""
          safeGlobToRegex' ('*':cs)       = prependStr ".*"           (safeGlobToRegex' cs)
          safeGlobToRegex' ('?':cs)       = prepend    '.'            (safeGlobToRegex' cs)
          safeGlobToRegex' ('[':'!':c:cs) = prependStr ('[':'^':c:[]) (charClass cs)
          safeGlobToRegex' ('[':c:cs)     = prependStr ('[':c:[])     (charClass cs)
          safeGlobToRegex' ('[':_)        = Left "Unterminated character class"
          safeGlobToRegex' (c:cs)         = prependStr (escape c)     (safeGlobToRegex' cs)
          
          charClass :: String -> Either GlobError String
          charClass (']':cs) = prepend ']' (safeGlobToRegex' cs)
          charClass (c:cs)   = prepend  c  (charClass cs)
          charClass []       = Left "Unterminated character class"
          
          prepend :: Char -> Either GlobError String -> Either GlobError String
          prepend c (Left  err) = Left  err
          prepend c (Right reg) = Right (c:reg)

          prependStr :: String -> Either GlobError String -> Either GlobError String
          prependStr str either = foldr prepend either str
          
          append :: Either GlobError String -> Char -> Either GlobError String
          append (Left  err   ) c = Left err
          append (Right string) c = Right (string++[c])

          escape :: Char -> String
          escape c | c `elem` regexChars = '\\' : [c]
                 | otherwise = [c]
            where regexChars = "\\+()^$.{}]|"

-- 2
safeNamesMatching :: String -> IO (Either GlobError [String])
safeNamesMatching pat
  | not (isPattern pat) = do
    exists <- doesNameExist pat
    return (if exists then Right [pat] else Right [])
  | otherwise = do
    case splitFileName pat of
      ("", baseName) -> do
          curDir <- getCurrentDirectory
          safeListMatches curDir baseName
      (dirName, baseName) -> do
          eitherDirs <- if isPattern dirName
                        then safeNamesMatching (dropTrailingPathSeparator dirName)
                        else return (Right [dirName])
          case eitherDirs of
            Left  err     -> return (Left err)
            Right dirs -> do
                let listDir = if isPattern baseName
                              then safeListMatches
                              else safeListPlain
                pathNames <- forM dirs $ \dir -> do
                                 eitherBaseNames <- listDir dir baseName
                                 case eitherBaseNames of
                                    Left  err       -> return (Left  err)
                                    Right baseNames -> return (Right (map (dir </>) baseNames))
                case pathNames of
                    (Left  err):errs -> return (Left err)
                    eitherPaths      -> return (Right (concat . map noEither' $ eitherPaths))
                                    where noEither' :: Either GlobError [String] -> [String]
                                          noEither' (Left  y) = [y]
                                          noEither' (Right x) = x

safeMatchesGlob :: FilePath -> String -> Either GlobError Bool
name `safeMatchesGlob` pat = case regex of 
        Left  err -> Left err
        Right reg -> Right (name =~ reg)
        where regex = safeGlobToRegex pat

safeListMatches :: FilePath -> String -> IO (Either GlobError [String])
safeListMatches dirName pat = do
    dirName' <- if null dirName
                then getCurrentDirectory
                else return dirName
    handle (const (return (Left "Bad path")):: IOError -> IO (Either GlobError [String])) $ do
        names <- getDirectoryContents dirName'
        let names' = if isHidden pat
                     then filter isHidden names
                     else filter (not . isHidden) names
        let results = func pat names'
                        where func :: String -> [String] -> [Either GlobError String]
                              func _    []     = []
                              func glob (n:ns) = case n `safeMatchesGlob` glob of
                                                    Left  err  -> [Left err] -- Stops abruptly at Left
                                                    Right bool -> case bool of
                                                                    True  -> (Right n) : (func glob ns)
                                                                    False -> func glob ns
        return (case results of
                [Left err] -> Left err -- If there is a Left in results it will be the first and only value
                matches    -> Right (map noEither results)
                                where noEither (Right x) = x
                                      noEither (Left  y) = error "This should never happen" -- See previous comment
                )
                
safeListPlain :: FilePath -> String -> IO (Either GlobError [String])
safeListPlain dirName baseName = do
    exists <- if null baseName
              then doesDirectoryExist dirName
              else doesNameExist (dirName </> baseName)
    return (if exists then Right [baseName] else Right [])
