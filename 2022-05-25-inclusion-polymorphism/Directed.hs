{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GHC2021                #-}
{-# LANGUAGE MonoLocalBinds         #-}
-- {-# LANGUAGE ConstraintKinds        #-}
-- {-# LANGUAGE DataKinds              #-}
-- {-# LANGUAGE FlexibleInstances      #-}
-- {-# LANGUAGE KindSignatures         #-}
-- {-# LANGUAGE RankNTypes             #-}
-- {-# LANGUAGE ScopedTypeVariables    #-}
-- {-# LANGUAGE TypeSynonymInstances   #-}

import           Data.Kind  (Constraint)
import           Data.Proxy (Proxy (..))


class Castable a b where
  cast1 :: a -> b

-- Law: Whenever (c x, c y) then z should be a member of class c
-- containing the union of x and y.
class Directed (c :: * -> Constraint) x y z | c x y -> z where
  castUnion :: Proxy c -> (x,y) -> (z,z)

-- Law: castOp c preserves associativity.
castOp ::
  forall c x y z.
  (Directed c x y z, c x, c y, c z) =>
  Proxy c ->
  (forall a. c a => a -> a -> a) ->
  x -> y -> z
castOp c op x y = let (zx,zy) = castUnion c (x, y) in op zx zy

instance Castable Integer Double where
  cast1 = fromInteger
instance Castable Rational Double where
  cast1 = fromRational
instance Castable Double Double where
  cast1 = id
instance (Castable a Double, Castable b Double) => Directed Num a b Double where
    castUnion _ (a, b) = (cast1 a, cast1 b)
genericPlus ::
  forall a b c.
  (Num a, Num b, Num c, Directed Num a b c) =>
  a -> b -> c
genericPlus = castOp (Proxy :: Proxy Num) (+)

main = do
  print $ show $ genericPlus (2 :: Integer) (5 :: Integer)
  print $ show $ genericPlus (1/3 :: Rational) (5 :: Integer)
  print $ show $ genericPlus (1/3 :: Rational) (2/3 :: Rational)
  print $ show $ genericPlus (4 :: Integer) (2/3 :: Rational)
