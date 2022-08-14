{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE UndecidableSuperClasses #-}

import           Data.Kind


class c a => IsAnyTypeOf (a :: Type) (c :: Type -> Constraint) | a -> c where
  mkAny :: c e => e -> a

class Show a => C a b

data X1 t = X1 t deriving Show
instance Show t => C (X1 t) t

data X2 t = X2 (t, t) deriving Show
instance Show t => C (X2 t) t

data AnyC t = forall a. C a t => MkAnyC a
deriving instance C (AnyC t) t
instance Show (AnyC t) where show (MkAnyC a) = show a

class c b a => MPTCFlip c a b
deriving instance C b a => MPTCFlip C a b

instance AnyC t `IsAnyTypeOf` (MPTCFlip C t) where
  mkAny = MkAnyC

main = do
  print $ show ([mkAny (X1 (2 :: Int)), mkAny (X2 ((4::Int), (2::Int)))] :: [AnyC Int])
  print "end"
