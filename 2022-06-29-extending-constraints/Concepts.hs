{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Concepts (MkAnyA(..), A(..), A1 (..), A2 (..), someAs) where

import           Data.Kind  (Constraint, Type)
import           Data.Proxy (Proxy (..))

-- | Extra constraints type family served as type class extension mechanism for the type
-- class MkAny aClass where
--   type family Any aClass = r | r -> aClass
--  mkAny :: aClass a => a -> Any aClass

-- instance {-# OVERLAPPABLE #-} MkAny a where
  -- type instance Any a = ()
  -- mkAny a = undefined

type family ExtraConstraint (a :: Type -> Constraint) :: Type -> Constraint
data ExistentialExtra a = ExistentialExtra a (Proxy (ExistentialExtra a))

--------------------------------------------------------------------------------
-- Concept-level
--
-- In this level, we want the definitions stripping off as much as irrelevant
-- details possible.
--------------------------------------------------------------------------------
-- * A as a conceptual-level type class needs not to know what integrations would be
--   added to the system-level instances of A.
-- * To allow future extensions, ExtraConstraints is provided as the unknown constraints
--   decided only in the system-level instances.
-- * GHC is paranoid about cyclic constraints, so it wants UndecidableSuperClasses.

-- NOTE: ExtraConstraint A a
class A a e where
  type AnyA_TF e :: Type
  foo :: a -> Int

-- * E.g. an existential type of all A-s needs not to know the actual constraints in the system-level
data AnyA e = forall a. A a e => MkAnyA a

-- * Let's define some instances
data A1 e = A1
instance A (A1 e) e where
  type AnyA_TF e = AnyA e
  foo _ = 1

data A2 e = A2
instance A (A2 e) e where
  type AnyA_TF e = AnyA e
  foo _ = 2

-- class A a => AnyAImpl a where
--   data family AnyA a :: Type
--   mkAny :: a -> AnyA a

--   someAs :: [AnyA a]
--   someAs = [mkAny A1, mkAny A2, mkAny A1]

-- class MkAny a where
--   -- * Let's define a function that uses these instances
--   someAs :: [Any A]
--   someAs = [mkAny A1, mkAny A2, mkAny A1]
