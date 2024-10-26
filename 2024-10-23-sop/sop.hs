#!/usr/bin/env -S cabal run -v1
{- cabal:
build-depends: base, sop-core
default-language: GHC2024
ghc-options: -Wall
-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

import           Data.Kind     (Type)
import           Data.SOP      (I, NP (Nil, (:*)))
import           Data.Typeable (Proxy (Proxy), Typeable, typeRep)
import           GHC.TypeNats  (KnownNat, Nat, fromSNat, pattern SNat, type (+))

{- Some NP utilities -}

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[]        ys = ys
  Append (x ': xs)  ys = x ': Append xs ys

-- | "Existential" data kind (?).
type NP' :: Type
data NP' where
  MkNP' :: forall k h l. NP (h :: k -> Type) (l :: [k]) -> NP'

type family NPToList (a :: NP') :: [Type] where
  NPToList (MkNP' Nil) = '[]
  NPToList (MkNP' (_ (a :: Type) :* as)) = a : NPToList (MkNP' as)
  NPToList (MkNP' ((as :: NP _ _) :* as')) = Append (NPToList (MkNP' as)) (NPToList (MkNP' as'))

{- = Quest 1: Counting number ('Nat' kind) of elements in NP type -}

{- == Solution 1.1: Using type family 'Length' -}

type family Length (xs :: [x]) :: Nat where
  Length ('[]) = 0
  -- Length (a:'[]) = Length a
  Length (a:as) = 1 + Length as

type CountNP :: forall k h l. NP (h :: k -> Type) (l :: [k]) -> Nat
type CountNP (a :: NP h l) = Length l

{- == Solution 1.2: Counting recursively using 'MkNP' data kind wrapper and 'NatBuilder' -}

type family CountNP2' (np :: NP') :: Nat where
  CountNP2' (MkNP' Nil)         = 0
  -- NOTE! This allows recursively examing into a1 if a1 is also a NP type.
  CountNP2' (MkNP' (a :* Nil)) = CountNP2' (MkNP' a)
  CountNP2' (MkNP' (_ :* as))  = 1 + CountNP2' (MkNP' as)

type CountNP2 :: forall k h l. NP (h :: k -> Type) (l :: [k]) -> Nat
type CountNP2 (a :: NP h l) = CountNP2' (MkNP' a)

testCountNP :: forall np np'.
             ( KnownNat (CountNP np), KnownNat (CountNP2 np)
             , Typeable np
             , np' ~ NPToList (MkNP' np)
             , Typeable np'
             ) => IO ()
testCountNP = do
    putStrLn $ show (typeRep (Proxy @np'))
    putStrLn $ "CountNP1 " <> show (fromSNat (SNat @(CountNP np)))
    putStrLn $ "CountNP2 " <> show (fromSNat (SNat @(CountNP2 np)))
    putStrLn ""

{- = Quest 2: Build a currying function type from NP -}

type family Fn (as :: [Type]) (r :: Type) :: Type where
  Fn '[]    r = r
  Fn (a:as) r = a -> Fn as r

class CFn (as :: [Type]) (f :: Type) where
    mkFn' :: (Maybe String -> String) -> f

instance CFn ('[]) String where
    mkFn' cb = cb Nothing

instance forall (a :: Type) (as :: [Type]) f'.
         ( Show a
         , f' ~ Fn as String
         , CFn as f'
         ) => CFn (a:as) (a -> f') where
  mkFn' cb = \a ->  mkFn' @as @(Fn as String)
                    (\b -> (cb . Just) (show a ++ (case b of Just b' -> ", " ++ b'; Nothing -> "")))

mkFnL :: forall (as :: [Type]) r.
       ( r ~ (Fn as String)
       , CFn as r
       ) => r
mkFnL = mkFn' @as @(Fn as String) (\case Just b' -> b'; Nothing -> "")

mkFn :: forall np as r.
      ( as ~ NPToList (MkNP' np)
      , r ~ (Fn as String)
      , CFn as r
      ) => r
mkFn = mkFnL @as

{- Main -}

main = do
   testCountNP @((I Int :* I Double :* Nil) :* Nil)
   testCountNP @(I Int :* I Double :* I Float :* Nil)
   testCountNP @(I Int :* I Double :* Nil)
   testCountNP @(I Int :* Nil)
   testCountNP @(Nil :: NP I '[])

   putStrLn $ mkFnL @'[Int] 42
   putStrLn $ mkFnL @'[Int, String] 42 "USA"
   putStrLn $ mkFn @(I Int :* I Double :* Nil) 42 4.2

{-
Local Variables:
fill-column: 100
haskell-process-type: cabal-repl
haskell-process-load-or-reload-prompt: t
End:
-}
