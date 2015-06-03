-- File: Real World Haskell/Ch24/Exercises.hs
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
    stata <- sequence $ map isEmptyMVar mvs
    let x = length $ takeWhile (==True) stata
    if x == 0
        then putMVar (mvs!! x   ) v
        else putMVar (mvs!!(x-1)) v


readBoundedChan :: BoundedChan a -> IO a
readBoundedChan (BoundedChan mva) = do
    mvs <- takeMVar mva
    putMVar mva mvs
    stata <- sequence $ map isEmptyMVar mvs
    let x = length $ takeWhile (==True) stata
    if x == length mvs
        then takeMVar $ mvs!!(x-1)
        else takeMVar $ mvs!!x


    -- NOT WORKING YET
boundedChanExample = do
  ch <- newBoundedChan 2
  forkIO $ do
    writeBoundedChan ch "hello world"
    writeBoundedChan ch "now i quit"
  readBoundedChan ch >>= print
  readBoundedChan ch >>= print
