module WasmComm where

import           Foreign.C.String
import           Store

foreign export ccall echo :: CString -> IO CString
echo a = peekCString a >>= newCString

foreign export ccall save :: CString -> CString -> IO ()
save k v = do
  k' <- peekCString k
  v' <- peekCString v
  save2 k' v'

foreign export ccall load :: CString -> IO CString
load k = peekCString k >>= load1 >>= newCString

foreign export ccall size :: IO Int
size = size2

-- | Notes
--
-- * https://stackoverflow.com/questions/22886845/holding-a-data-map-in-memory
-- * https://gitlab.haskell.org/ghc/ghc/-/issues/22468
-- * https://wiki.haskell.org/Top_level_mutable_state
