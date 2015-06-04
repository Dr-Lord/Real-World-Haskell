-- File: Real World Haskell/Ch24/Exercises.hs
{-# LANGUAGE BangPatterns #-}
import Control.Concurrent
import Control.Monad (replicateM)

-- 1
data BoundedChan a = BoundedChan (MVar [MVar a])

newBoundedChan :: Int -> IO (BoundedChan a)
newBoundedChan n = do
    mvs <- replicateM n newEmptyMVar
    mva <- newMVar mvs
    return $ BoundedChan mva


writeBoundedChan :: BoundedChan a -> a -> IO ()
writeBoundedChan bc@(BoundedChan mva) v = do
    mvs <- takeMVar mva
    putMVar mva mvs
    stati <- sequence $ map isEmptyMVar mvs
    let x = length $ takeWhile (==True) stati
    if x == 0
        then putMVar (mvs!! x   ) v
        else putMVar (mvs!!(x-1)) v


readBoundedChan :: BoundedChan a -> IO a
readBoundedChan (BoundedChan mva) = do
    mvs <- takeMVar mva
    putMVar mva mvs
    stati <- sequence $ map isEmptyMVar mvs
    let x = length $ takeWhile (==True) stati
    if x == length mvs
        then takeMVar $ mvs!!(x-1)
        else takeMVar $ mvs!!x


boundedChanExample = do
  ch <- newBoundedChan 2
  forkIO $ do
    writeBoundedChan ch "hello world"
    writeBoundedChan ch "now i quit"
  readBoundedChan ch >>= print
  readBoundedChan ch >>= print

-- 2
-- The Bang Pattern Language Pragma has to be at the beginning of the file

newtype MVarStrict a = MVarStrict (MVar a)

newMVarStrict :: a -> IO (MVarStrict a)
newMVarStrict v = do
    let !v' = v
    mv <- newMVar v'
    return $ MVarStrict mv


putMVarStrict :: MVarStrict a -> a -> IO ()
putMVarStrict (MVarStrict mv) v = do
    let !v' = v
    putMVar mv v'


takeMVarStrict :: MVarStrict a -> IO a
takeMVarStrict (MVarStrict mv) = do
    !v <- takeMVar mv
    return v

