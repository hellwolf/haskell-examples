{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Synopsis: to produce a proof that a constraint `k` has at least two distinct instances using type families.
--
--   The usage of AllowAmbiguousTypes is optional, if you are willing to carry around the proxy phantom type.

import           Data.Coerce (Coercible)
import           Data.Kind   (Constraint, Type)
import           Data.Proxy  (Proxy (..))
import           Data.Void   (Void)

data DistinctInstancesTrue k a b = DistinctInstancesTrue
class DistinctInstancesTrueC k a b
instance (k a, k b) => DistinctInstancesTrueC (DistinctInstancesTrue k a b) a b

type family DistinctInstances (k :: Type -> Constraint) (a :: Type) (b :: Type) where
  DistinctInstances k a a = Void
  DistinctInstances k a b = DistinctInstancesTrue k a b

distinctProduct :: forall k a b. DistinctInstancesTrueC k a b => a -> b -> (a, b)
distinctProduct = (,)

main = do
  print $ distinctProduct @(DistinctInstances Num Int Float) (1 :: Int) (1.0 :: Float)
  -- print $ distinctProduct (1 :: Int) (1.0 :: Float)
  -- print $ distinctProduct @(DistinctInstances Num Int Int) (1 :: Int) (1 :: Int)
  -- print $ distinctProduct @(DistinctInstances Num Int Bool) (1 :: Int) True
