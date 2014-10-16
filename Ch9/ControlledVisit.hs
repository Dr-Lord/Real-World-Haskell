-- File: Real World Haskell\Ch09\ControlledVisit.hs
module ControlledVisit where

import Control.Monad (liftM, forM)
import System.Directory (Permissions(..), getModificationTime, getPermissions, getDirectoryContents)
import Data.Time.Clock (UTCTime(..))
import Control.Exception (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import System.FilePath ((</>))

import Data.List (sortBy) -- Necessary for Ex 1
import Data.Ord (comparing) -- Necessary for Ex 1
import System.FilePath (takeExtension) -- Necessary for Ex 3

data Info = Info {
      infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe UTCTime
    } deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info
getInfo path = do
    perms <- maybeIO (getPermissions path)
    size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
    modified <- maybeIO (getModificationTime path)
    return (Info path perms size modified)

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle ((\_ -> return Nothing) :: IOError -> IO (Maybe a)) (Just `liftM` act)

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
    names <- getUsefulContents path
    contents <- mapM getInfo (path : map (path </>) names)
    liftM concat $ forM (order contents) $ \info -> do
      if isDirectory info && infoPath info /= path
        then traverse order (infoPath info)
        else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms


-- 1
reverseAlph :: [Info] -> [Info]
reverseAlph = reverse . sortBy (comparing infoPath)

-- 2
contentsFirst :: [Info] -> [Info]
contentsFirst (x:xs) = xs ++ [x]

-- 3
pathP :: Info -> FilePath
pathP = infoPath

sizeP :: Info -> Integer
sizeP info = case infoSize info of
    Just size -> size
    Nothing   -> -1

liftPath :: (FilePath -> a) -> Info -> a
liftPath f = f . infoPath

-- myTest info = takeExtension (infoPath info) == ".cpp" && (infoSize info) > 131072

liftP :: (a -> b -> c) -> (Info -> a) -> b -> (Info -> c)
liftP operator f val info = f info `operator` val

eqP :: (Eq a) => (Info -> a) -> a -> (Info -> Bool)
eqP = liftP (==)

gtP, ltP :: (Ord a) => (Info -> a) -> a -> (Info -> Bool)
gtP = liftP (>)
ltP = liftP (<)

liftP2 :: (a -> b -> c) -> (Info -> a) -> (Info -> b) -> (Info -> c)
liftP2 operator f g info = f info `operator` g info

andP, orP :: (Info -> Bool) -> (Info -> Bool) -> (Info -> Bool)
andP = liftP2 (&&)
orP  = liftP2 (||)

-- myTest = (liftPath takeExtension `equalP` ".cpp") `andP` (sizeP `greaterP` 131072)

(==?) :: (Eq a) => (Info -> a) -> a -> (Info -> Bool)
(==?) = eqP
infix 4 ==?

(>?), (<?) :: (Ord a) => (Info -> a) -> a -> (Info -> Bool)
(>?) = gtP
(<?) = ltP
infix 4 >?, <?

(&&?), (||?) :: (Info -> Bool) -> (Info -> Bool) -> (Info -> Bool)
(&&?) = andP
(||?) = orP
infixr 3 &&?, ||?

-- myTest = liftPath takeExtension ==? ".cpp" &&? sizeP >? 131072

-- 4
traverseFind :: (Info -> Bool) -> ([Info] -> [Info]) -> FilePath -> IO [Info]
traverseFind predicate order path = filter predicate `liftM` traverse order path
