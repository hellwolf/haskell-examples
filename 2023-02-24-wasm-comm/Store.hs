{- cabal:
build-depends: base, containers
-}

module Store (save1, load1, size1, save2, load2, size2) where

import           Control.Concurrent.MVar
import           Data.IORef
import qualified Data.Map                as M
import           System.IO.Unsafe

_m1 :: IORef (M.Map String String)
{-# NOINLINE _m1 #-}
_m1 = unsafePerformIO $ newIORef M.empty
save1 k v = modifyIORef _m1 $ M.insert k v
load1 k = M.findWithDefault "" k <$> readIORef _m1
size1 = M.size <$> readIORef _m1


_m2 :: MVar (M.Map String String)
{-# NOINLINE _m2 #-}
_m2 = unsafePerformIO $ newMVar M.empty
save2 k v = modifyMVar_ _m2 $ pure . M.insert k v
load2 k = M.findWithDefault "" k <$> readMVar _m2
size2 = M.size <$> readMVar _m2

test = do
  save1 "a" "42"
  save1 "b" "32"
  print =<< size1
  print =<< load1 "a"
  print =<< load1 "b"
  print =<< load1 "c"

  save2 "a" "42"
  save2 "b" "32"
  print =<< size2
  print =<< load2 "a"
  print =<< load2 "b"
  print =<< load2 "c"
