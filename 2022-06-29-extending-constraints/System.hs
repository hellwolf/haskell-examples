{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

import           Concepts

--------------------------------------------------------------------------------
-- System-level
--
-- In this level, we apply the concepts and add integration to them.
--------------------------------------------------------------------------------

-- * E.g. We integrate the "Show" type class for all A-s
-- type instance ExtraConstraint A = Show

data AnyA = forall a. (Show a, A a) => MkAnyA a
instance {-# OVERLAPPING #-} MkAny A where
  type instance Any A = AnyA
  mkAny :: A a => a -> Any A
  mkAny a = MkAnyA a

-- * Show instances need to be defined, otherwise GHC would throw "No insance" errors
instance {-# INCOHERENT #-} A a => Show a where show _ = "A?"
instance Show A1 where show _ = "A1"
instance Show A2 where show _ = "A2"

-- * Now the system can comfortably have a Show instance for the AnyA
-- * Note that A did not need to know Show was required constraints in the system-level
-- instance Show AnyA where show (MkAnyA a) = show aE ++ ":" ++ (show $ foo a')
--                            where aE = existentialExtra a
--                                  (ExistentialExtra a' proxy) = aE
instance Show AnyA where show (MkAnyA a) = show a ++ ":" ++ (show $ foo a)

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
