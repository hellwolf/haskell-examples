{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableSuperClasses #-}

import           Data.Kind (Constraint, Type)


-- | Extra constraints type family served as type class extension mechanism for the type
type family ExtraConstraint (a :: Type -> Constraint) :: Type -> Constraint

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
class (ExtraConstraint A a) => A a where

-- * E.g. an existential type of all A-s needs not to know the actual constraints in the system-level
data AnyA = forall a. A a => MkAnyA a

-- * Let's define some instances
data A1 = A1
instance A A1

data A2 = A2
instance A A2

-- * Let's define a function that uses these instances
someAs :: [AnyA]
someAs = [MkAnyA A1, MkAnyA A2, MkAnyA A1]

--------------------------------------------------------------------------------
-- System-level
--
-- In this level, we apply the concepts and add integration to them.
--------------------------------------------------------------------------------

-- * E.g. We integrate the "Show" type class for all A-s
type instance ExtraConstraint A = Show

-- * Show instances need to be defined, otherwise GHC would throw "No insance" errors
instance Show A1 where show _ = "A1"
instance Show A2 where show _ = "A2"

-- * Now the system can comfortably have a Show instance for the AnyA
-- * Note that A did not need to know Show was required constraints in the system-level
instance Show AnyA where show (MkAnyA a) = show a

main :: IO ()
main = do
  (putStrLn . show) someAs

--
-- References:
--
-- * Discussion of this code: https://discourse.haskell.org/t/provide-extra-constraints-in-later-integration-stage-of-the-development/4713
--
-- * "a well-known problem in Haskell: how to define a type class such that instances of the type class can define
--   the type class constraints they require for the type class's operations."
--   "Why is Data.Set not an instance of Functor?"
--   * https://stackoverflow.com/questions/25422342/arbitrary-class-constraints-when-implementing-type-classes-in-haskell
--   * https://dorchard.wordpress.com/2011/09/22/constraint-kinds-in-haskell-finally-bringing-us-constraint-families/
-- * Other related:
--   * https://stackoverflow.com/questions/12397751/adding-class-constraints-to-typeclass-instance
--   * https://stackoverflow.com/questions/9598637/howto-further-constrain-an-existing-type-class-in-haskell

