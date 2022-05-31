-- GHC version: 9.2.2
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GHC2021             #-}
-- {-# LANGUAGE Haskell2010         #-}
-- {-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

import           Data.Kind     (Type)
import           Data.Proxy    (Proxy (..))
import           Data.Typeable (Typeable)


class (Typeable k, Typeable a) => Typeable' (a :: k) where
  tag :: Proxy a -> String

tagFromValue :: forall a. Typeable' a => a -> String
tagFromValue _ = tag (Proxy @a)

class Typeable' a => Typeable'' a where
  type T1 a :: Type
  type T2 a :: Type

-- type A :: Type -> Type
data A a = A
  { val1 :: T1 a
  , val2 :: T2 a
  }

-- * Does not work in GHC2021, but Haskell2010
-- Update: https://discourse.haskell.org/t/how-can-i-refactor-this-simple-recursive-function/4570/12
--         Adding kind signature to A solved the problem
instance Typeable'' a => Typeable' (A a) where
-- * Works in GHC2021
-- instance (Typeable'' a, Typeable (A a)) => Typeable' (A a) where
   tag _ = "A/" ++ tag (Proxy @a)

data B
instance Typeable' B where
  tag _ = "B"
instance Typeable'' B where
  type T1 B = String
  type T2 B = Int

main :: IO ()
main = do
  let a = A "hello" 42 :: A B
  putStrLn $ tagFromValue a
  putStrLn $ tag (Proxy @(A B)) ++ ": " ++ show(val1 a) ++ show(val2 a)
