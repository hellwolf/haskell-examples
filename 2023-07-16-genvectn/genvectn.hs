#!/usr/bin/env cabal
{- cabal:
default-language: GHC2021
ghc-options: -Wall
build-depends: base >=4.18, constraints, QuickCheck
-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE PatternSynonyms    #-}

{-|

Origin: https://discourse.haskell.org/t/how-to-create-arbitrary-instance-for-dependent-types/6990/31
Problem: QuickCheck-style generator for arbitary Length-indexed vector.

The QuickCheck library provides a function 'vectorOf' with the following signature:

@
-- | Generates a list of the given length.
vectorOf :: Int -> Gen a -> Gen [a]
@

We trust the code generates a list of "a"s with the length provided. But can we do better to encode the length guarantee
in type?

We know how to define length-indexed vector, say @Vect :: Nat -> Type -> Type@. A QuickCheck generator for it could look
like:

@
-- | function to generate arbitrary values for a lenght-n vector.
vect_n_of :: forall {a} (n :: Nat) . KnownNat n => Gen a -> Gen (Vect n a)
@

Upon close inspection, we can see that the type information encodes that the generated vector is guaranteed to have n
elements. That is a step-up from its inferior 'vectorOf' variant. But the flip side is that it becomes harder to
construct the correct implementation of this type.

This annotated Haskell script (invokable with "cabal run") provides an example of how it should be implemented. It uses
the "constraints" library for the required machinery working with constraints, including arithmetic for 'KnownNat'.

-}

import           Data.Constraint     (Dict (Dict), (:-) (Sub), (\\))
import           Data.Constraint.Nat (minusNat)
import           Data.Kind           (Type)
import           Data.Type.Ord       (Compare)
import           GHC.TypeLits
    ( KnownNat
    , Nat
    , OrderingI (..)
    , cmpNat
    , natSing
    , pattern SNat
    , type (+)
    , type (-)
    , type (<=)
    , withSomeSNat
    )
import           Test.QuickCheck     (Arbitrary (..), Gen, sample)
import           Unsafe.Coerce       (unsafeCoerce)


-- | A version of length indexed vector.
data Vect :: Nat -> Type -> Type where
  VNil  :: Vect 0 a
  -- NOTE: Using @(n - 1)@ to makes sure that there is an evidence of @1 <= n@.
  VCons :: a -> Vect (n - 1) a -> Vect n a

deriving instance Show a => Show (Vect n a)

-- | Provide axiomatic proof, make sure you wrote it on paper!
axiom :: Dict c
axiom = unsafeCoerce (Dict :: Dict ())

-- | Axiom: For natural numbers, if m > n then n + 1 <= m.
--
--   Note: 1) more unmerged axioms available at: https://github.com/ekmett/constraints/issues/63, e.g. flipGT ltIsSuccLe.
--         2) To avoid introducing AllowAmbiguousTypes in this small example, proxy types are used instead.
nat_gt_is_flipsucclte :: forall proxy1 proxy2 (m :: Nat) (n :: Nat) . proxy1 m -> proxy2 n -> (Compare m n ~ 'GT) :- (n + 1 <= m)
nat_gt_is_flipsucclte _ _ = Sub axiom

-- | Helper function to generate arbitrary values for a lenght-n vector.
vect_n_of :: forall {a} (n :: Nat) . KnownNat n => Gen a -> Gen (Vect n a)
vect_n_of g = case cmpNat (natSing @n) (natSing @0) of
    EQI -> pure VNil -- Note: with the cmpNat pattern matching, GHC has no trouble to infer @KnownNat 0@.
    GTI -> -- Note: in order to use recursively call @vect_n_f@, we must prove @KnownNat (n - 1)@.
      --   --       We can either use 'withDict', or its operator form '\\'. Note the order of '\\' when chaining evidences.
      --
      --   Alternatively:
      --  @withDict (nat_gt_is_flipsucclte @n @0) $ withDict (minusNat @n @1) $@
      VCons <$> g <*> vect_n_of g
              -- Note: with the evidence 1<=n, we can prove n-1 is KnownNat too.
              \\ minusNat @n @1
              -- Note: now we have evidence that @(Compare n 0 ~ GT)@ (n>0), let's hand wave and prove 1<=n.
              \\ nat_gt_is_flipsucclte (natSing @n) (natSing @0)
    LTI -> error "Nothing less than zero"

instance (KnownNat n, Arbitrary a) => Arbitrary (Vect n a) where
    -- arbitrary :: Gen (Vect n a)
    arbitrary = vect_n_of arbitrary

main :: IO ()
main = do
  -- Trying some compile-time known nats...
  putStrLn "> arbitrary :: Gen (Vect 0 ())"
  sample (arbitrary :: Gen (Vect 0 ()))
  putStrLn "> arbitrary :: Gen (Vect 1 Int)"
  sample (arbitrary :: Gen (Vect 1 Int))
  putStrLn "> arbitrary :: Gen (Vect 2 Float)"
  sample (arbitrary :: Gen (Vect 2 Float))
  putStrLn "> arbitrary :: Gen (Vect 1 String)"
  sample (arbitrary :: Gen (Vect 4 String))
  -- Now an user input nat...
  putStrLn "> How many elements do you want?"
  n <- readLn :: IO Integer
  withSomeSNat n $ \case
    -- Note: you have to use @SNat@ pattern matching to bring @n@ to the scope, since only @natSing@ can give the
    --       evidence of @KnownNat n@.
    (Just (SNat @n)) -> sample (arbitrary :: Gen (Vect n Char))
    _                -> putStrLn "Not all integers are born SNats"
