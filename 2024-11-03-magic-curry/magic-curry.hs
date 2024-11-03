#!/usr/bin/env -S cabal run -v1
{- cabal:
build-depends: base
default-language: GHC2024
ghc-options: -Wall
-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

import           Debug.Trace (trace)

-- | Variable-arity currying helper for any function wrapper @f@ that takes curries a @a@ and returns a @b@.
class MagicCurry f a b where
  curry' :: f -> a -> b

-- | Simple function wrapper for any Husk functions, whose `MagicCurry` instances traces all its arguments.
data Fn f where MkFn :: forall f. f -> Fn f

instance {-# OVERLAPPABLE #-} (Show a, Show b) => MagicCurry (Fn (a -> b)) a b where
  curry' (MkFn f) a = trace (show a) (f a)

instance (Show a, MagicCurry (Fn (b -> c)) b c) => MagicCurry (Fn (a -> b -> c)) a (b -> c) where
  curry' (MkFn f) a b = trace (show a) (curry' (MkFn (f a)) b)

{- DEMO -}

foo :: String -> Int -> Bool -> String
foo l i b = if b then l ++ ": " ++ show i else l ++ ": nah"

main = do
  let foo' = MkFn foo
  putStrLn (curry' foo' "Hello: " (42 :: Int) True)
  putStrLn (curry' foo' "Hello: " (69 :: Int) False)

{-
Local Variables:
fill-column: 100
haskell-process-type: cabal-repl
haskell-process-load-or-reload-prompt: t
End:
-}
