#!/usr/bin/env -S cabal run -v1 --
{-

This was prompted and inspired by https://0xd34df00d.me/posts/2024/09/naive-nfas.html#linear-vectors
to write a faster linear-type based NFA match function using a mutable vector of unboxed data.

It is made especially to compare fairly with the ST monad version of the NFA match function
mentioned in the article.


How to use:

```
$ make bench
$ ./nfa-perf.hs linear2 $$i.dat +RTS -s
$ ./nfa-perf.hs st $$i.dat +RTS -s
```

Full post:
https://discourse.haskell.org/t/linearly-typed-mutable-vector-of-unboxed-values-hack-for-nfa-benchmark/11787

-}
{- cabal:
build-depends: base, vector, bytestring, mmap,
               linear-base, array
ghc-options: -Wall -O2 -rtsopts
-}
{-# LANGUAGE GHC2024 #-}
-- {-# LANGUAGE Strict #-} -- Let's practice findubg the strictness by looking at code...
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

import Data.Vector.Unboxed.Mutable qualified as VM
import Data.ByteString qualified as BS
import Data.Vector qualified as V
import System.IO.MMap (mmapFileByteString)
import System.Environment (getArgs)
import Data.Word (Word32, Word8)
import Control.Applicative (Alternative (..))
-- baseline match
import Data.Functor ((<&>))
-- ST match
import Control.Monad.ST
-- linear
import Prelude.Linear qualified as L
import Data.Vector.Mutable.Linear qualified as VL
-- linear unboxed
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Linear qualified as L


--
-- NFA bits
--
type StateId q = (Integral q, Enum q)

data Trans q
  = TEps q
  | TBranch q q
  | TCh Word8 q
  deriving (Eq, Show)

type TransMap q = V.Vector (Trans q)

getTrans :: StateId q => q -> TransMap q -> Trans q
getTrans q m = m V.! fromIntegral q

data NFA q = NFA
  { transitions :: TransMap q
  , initState :: q
  , finState :: q
  }
data MatchResult a = SuccessAt a | Failure
  deriving (Eq, Ord, Show, Functor)

instance Applicative MatchResult where
  SuccessAt f <*> SuccessAt v = SuccessAt $ f v
  _           <*> _           = Failure
  pure = SuccessAt

instance Alternative MatchResult where
  SuccessAt a <|> ~_ = SuccessAt a
  _           <|> ~r = r
  empty = Failure

--
-- match variants
--

-- baseline match function
--

match :: StateId q => NFA q -> BS.ByteString -> MatchResult Int
match NFA{..} bs = go initState 0
  where
  go q i | q == finState = SuccessAt i
  go q i = case q `getTrans` transitions of
             TEps q' -> go q' i
             TBranch q1 q2 -> go q1 i <|> go q2 i
             TCh ch q'
               | bs `BS.indexMaybe` i == Just ch -> go q' (i + 1)
               | otherwise -> Failure

-- ST Monad match function
--

match_st :: (VM.Unbox q, StateId q) => NFA q -> BS.ByteString -> MatchResult Int
match_st NFA{..} bs = runST $ do
  stack <- VM.unsafeNew 24_000_000
  let go s q i
        | q == finState = pure $ SuccessAt i
        | otherwise = case q `getTrans` transitions of
              TEps q' -> go s q' i
              TBranch q1 q2 -> do VM.unsafeWrite stack s (q2, i)
                                  go (s + 1) q1 i
              TCh ch q'
                | bs `BS.indexMaybe` i == Just ch -> go s q' (i + 1)
                | s == 0 -> pure Failure
                | otherwise -> do (q'', i'') <- VM.unsafeRead stack (s - 1)
                                  go (s - 1) q'' i''
  go 0 initState 0

-- Linear match function with boxed mutable vector
--

match_linear1 :: forall q. StateId q => NFA q -> BS.ByteString -> MatchResult Int
match_linear1 NFA{..} bs = L.unur L.$ VL.empty L.$ go initState 0
  where
  -- go :: q -> Int -> VL.Vector (q, Int) %1-> L.Ur (MatchResult Int)
  go q i stack
    | q == finState = stack `L.lseq` L.Ur (SuccessAt i)
    | otherwise = case q `getTrans` transitions of
                    TEps q' -> go q' i stack
                    TBranch q1 q2 -> go q1 i L.$ (q2, i) `VL.push` stack
                    TCh ch q'
                      | bs `BS.indexMaybe` i == Just ch -> go q' (i + 1) stack
                      | otherwise -> case VL.pop stack of
                                      (L.Ur top, stack'')
                                        | (Just (q'', i'')) <- top -> go q'' i'' stack''
                                        | otherwise -> stack'' `L.lseq` L.Ur Failure

-- Linear match function with unboxed mutable vector
--

newtype LinearIOVector a = MkLinearIOVector (L.Ur (VM.IOVector a))

instance L.Consumable (LinearIOVector a) where
  consume (MkLinearIOVector ur) = L.consume ur

new_linear_iovec :: VM.Unbox a => Int -> LinearIOVector a
new_linear_iovec = unsafePerformIO . (MkLinearIOVector . L.Ur <$>) . VM.unsafeNew

read_linear_iovec :: VM.Unbox a => LinearIOVector a %1 -> Int -> (L.Ur a, LinearIOVector a)
read_linear_iovec (MkLinearIOVector (L.Ur vec)) i = L.toLinear unsafePerformIO $ do
  a <- (L.toLinear VM.unsafeRead) vec i
  pure (L.Ur a, MkLinearIOVector (L.Ur vec))

write_linear_iovec :: VM.Unbox a => LinearIOVector a %1 -> Int -> a -> LinearIOVector a
write_linear_iovec (MkLinearIOVector (L.Ur vec)) i a = L.toLinear unsafePerformIO $ do
  (L.toLinear VM.unsafeWrite) vec i a >> pure (MkLinearIOVector (L.Ur vec))

match_linear2 :: forall q. (VM.Unbox q, StateId q) => NFA q -> BS.ByteString -> MatchResult Int
match_linear2 NFA{..} bs = L.unur L.$ go initState 0 (new_linear_iovec 24_000_000) 0
  where
    go q i !stack s
      | q == finState = stack `L.lseq` L.Ur (SuccessAt i)
      | otherwise = case q `getTrans` transitions of
                      TEps q' -> go q' i stack s
                      TBranch q1 q2 -> go q1 i (write_linear_iovec stack s (q2, i)) (s + 1)
                      TCh ch q'
                        | bs `BS.indexMaybe` i == Just ch -> go q' (i + 1) stack s
                        | s == 0 -> pure Failure
                        | otherwise -> let (L.Ur (q'', i''), stack'') = read_linear_iovec stack (s - 1)
                                       in go q'' i'' stack'' (s - 1)

-- Testing code:
--

nfa :: NFA Word32
nfa = NFA {..}
  where
  initState = 0
  finState = 13
  transitions = V.fromList
    [ TBranch 2 1
    , TEps 12
    , TBranch 4 8
    , TEps 0
    , TCh a 5
    , TEps 6
    , TCh a 7
    , TEps 3
    , TCh a 9
    , TEps 10
    , TCh b 11
    , TEps 3
    , TCh z 13
    ]
  a = 97
  b = 98
  z = 122

main :: IO ()
main = do
  (t, path) <- getArgs <&> \case [t, path] -> (t, path)
                                 _ -> error "wrong usage"
  str <- mmapFileByteString path Nothing
  case t of
    "st" -> print $ (t, path, match_st nfa str)
    "linear1" -> print $ (t, path, match_linear1 nfa str)
    "linear2" -> print $ (t, path, match_linear2 nfa str)
    _ -> print $ ("baseline", path, match nfa str)

{-
Local Variables:
fill-column: 100
haskell-process-type: cabal-repl
haskell-process-load-or-reload-prompt: t
End:
-}
