-- File: Real World Haskell\Ch09\BetterPredicate.hs
import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import Data.Time.Clock (UTCTime(..))
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import RecursiveContents (getRecursiveContents)

type InfoP a =  FilePath        -- path to directory entry
             -> Permissions     -- permissions
             -> Maybe Integer   -- file size (Nothing if not file)
             -> UTCTime         -- last modified
             -> a

type Predicate = InfoP Bool

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle ((\_ -> return Nothing) :: IOError -> IO (Maybe Integer)) $
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
    where check name = do
            perms <- getPermissions name
            size <- getFileSize name
            modified <- getModificationTime name
            return (p name perms size modified)

-- myTest path _ (Just size) _ = takeExtension path == ".cpp" && size > 131072
-- myTest _ _ _ _ = False

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing     _ = -1

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k
-- liftP q f k = \w x y z -> f w x y z `q` k  -- Same as above because of automatic Currying
-- liftP q = \f k -> \w x y z -> f w x y z `q` k -- Even more Currying
-- liftP :: (a -> b -> c) -> (InfoP a -> b -> InfoP c) -- Perhaps clearer with parentheses

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP = liftP (==)

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z
-- liftP could be rewritten in terms of liftP2, since this one is more general:
-- constP :: a -> InfoP a
-- constP k _ _ _ _ = k
-- liftP q f k = liftP2 q f (constP k)

andP = liftP2 (&&)
orP = liftP2 (||)

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

-- myTest2 = (liftPath takeExtension `equalP` ".cpp") `andP` (sizeP `greaterP` 131072)

(==?) :: (Eq a) => InfoP a -> a -> InfoP Bool
(==?) = equalP
(>?), (<?) :: (Ord a) => InfoP a -> a -> InfoP Bool
(>?)  = greaterP
(<?)  = lesserP
(&&?) = andP
(||?) = orP

-- myTest3 = (liftPath takeExtension ==? ".cpp") &&? (sizeP >? 131072)

infix  4 ==?
infixr 3 &&?
infixr 3 ||?
infix  4 >?
infix  4 <?

-- myTest4 = liftPath takeExtension ==? ".cpp" &&? sizeP >? 131072

