-- File: Real World Haskell/Ch24/ModifyMVarStrict.hs
{-# LANGUAGE BangPatterns #-}

import Control.Concurrent (MVar, putMVar, takeMVar)
import Control.Exception (block, onException, unblock)
import Prelude hiding (catch) -- use Control.Exception's version

modifyMVar_strict :: MVar a -> (a -> IO a) -> IO ()
modifyMVar_strict m io = block $ do
  a <- takeMVar m
  !b <- onException (unblock (io a)) (putMVar m a)
  putMVar m b
