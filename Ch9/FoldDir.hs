-- File: Real World Haskell\Ch09\FoldDir.hs
import ControlledVisit
import System.FilePath ((</>), takeFileName, takeExtension)
import Data.Char (toLower)
import Data.List (sortBy)

import System.Directory (doesFileExist) -- Necessary for Ex 2
import Control.Monad (liftM) -- Necessary for Ex 2

data Iterate seed = Done     { unwrap :: seed }
                  | Skip     { unwrap :: seed }
                  | Continue { unwrap :: seed }
                    deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed

foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = do
    endSeed <- fold initSeed path
    return (unwrap endSeed)
  where
    fold seed subpath = getUsefulContents subpath >>= walk seed

    walk seed (name:names) = do
      let path' = path </> name
      info <- getInfo path'
      case iter seed info of
        done@(Done _) -> return done
        Skip seed'    -> walk seed' names
        Continue seed'
          | isDirectory info -> do
              next <- fold seed' path'
              case next of
                done@(Done _) -> return done
                seed''        -> walk (unwrap seed'') names
          | otherwise -> walk seed' names
    walk seed _ = return (Continue seed)


atMostThreePictures :: Iterator [FilePath]
atMostThreePictures paths info
    | length paths == 3
      = Done paths
    | isDirectory info && takeFileName path == ".svn"
      = Skip paths
    | extension `elem` [".jpg", ".png"]
      = Continue (path : paths)
    | otherwise
      = Continue paths
  where extension = map toLower (takeExtension path)
        path = infoPath info

countDirectories :: Iterator Int
countDirectories count info =
    Continue (if isDirectory info
              then count + 1
              else count)


-- 1
sortAndFoldTree :: Iterator a -> a -> FilePath -> ([String] -> [String]) -> IO a
sortAndFoldTree iter initSeed path sortCriteria = do
    endSeed <- fold initSeed path
    return (unwrap endSeed)
  where
    fold seed subpath = getUsefulContents subpath >>= (walk seed . sortCriteria)
    
    walk seed (name:names) = do
      let path' = path </> name
      info <- getInfo path'
      case iter seed info of
        done@(Done _) -> return done
        Skip seed'    -> walk seed' names
        Continue seed'
          | isDirectory info -> do
              next <- fold seed' path'
              case next of
                done@(Done _) -> return done
                seed''        -> walk (unwrap seed'') names
          | otherwise -> walk seed' names
    walk seed _ = return (Continue seed)
    
-- 2
foldTreeTraversing :: Iterator a -> a -> FilePath -> Bool -> IO a
foldTreeTraversing iter initSeed path preOrPostOrder = do
    endSeed <- fold initSeed path
    return (unwrap endSeed)
  where
    fold seed subpath = getUsefulContents subpath >>= decideOrd preOrPostOrder >>= walk seed
        where decideOrd :: Bool -> [String] -> IO [String]
              decideOrd ord names
                | ord == True = return names
                | otherwise   = foldr order (return [] :: IO [String]) names              
              order :: String -> IO [String] -> IO [String]
              order name acc = do
                isFile <- doesFileExist name
                sorted <- acc
                if isFile
                    then return (name:sorted)
                    else return (sorted++[name])
    
    walk seed (name:names) = do
      let path' = path </> name
      info <- getInfo path'
      case iter seed info of
        done@(Done _) -> return done
        Skip seed'    -> walk seed' names
        Continue seed'
          | isDirectory info -> do
              next <- fold seed' path'
              case next of
                done@(Done _) -> return done
                seed''        -> walk (unwrap seed'') names
          | otherwise -> walk seed' names
    walk seed _ = return (Continue seed)

-- 3
constI :: Iterator a
constI seed _ = Continue seed

foldlStepI :: (Info -> a -> a) -> Iterator a
foldlStepI step seed info = Continue (step info seed)

ifThenElseI :: (Info -> Bool) -> Iterator a -> Iterator a -> Iterator a
ifThenElseI predicate thenI elseI seed info =
    if predicate info
        then thenI seed info
        else elseI seed info

caseI :: (Eq b) => (Info -> b) -> [(b, Iterator a)] -> Iterator a
caseI predicate hash seed info = case lookup (predicate info) hash of
    Just x  -> x seed info
    Nothing -> Continue seed