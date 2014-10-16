-- File: Real World Haskell\Ch09\SimpleFinder.hs
import RecursiveContents (getRecursiveContents)
import System.FilePath (takeExtension)
import System.Directory

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = do
  names <- getRecursiveContents path
  return (filter p names)