{-# OPTIONS_GHC -Wall -Wno-missing-signatures #-}
{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE GHC2021                #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE TypeFamilyDependencies #-}

import           Data.Functor.Identity (Identity (Identity, runIdentity))
import           Data.Int              (Int8)
import           Data.Kind             (Type)
import           Data.Maybe            (fromJust)

----------------------------------------------------------------------------------------------------

-- * The RebindableCase machineries

-- | Define a 'Caseable' instance for your functor @m@ and its type @a@.
class Caseable a (m :: Type -> Type) where
  type family LiftedCase m r = result | result -> m r
  type family UnliftedCase m r = result | result -> m r
  mkCaseable :: forall. UnliftedCase m a -> m a
  unCaseable :: forall. m a -> UnliftedCase m a
  matchCases :: forall b. m a -> (LiftedCase m a -> UnliftedCase m b) -> UnliftedCase m b

-- | The case matching combinator for all caseables.
match :: forall a b m. (Caseable a m, Caseable b m)
      => UnliftedCase m a -> (LiftedCase m a -> UnliftedCase m b) -> UnliftedCase m b
match a f = matchCases (mkCaseable a) f

----------------------------------------------------------------------------------------------------

-- * All husk objects could be mapped to an identity functor trivially

instance Caseable a Identity where
  type instance LiftedCase Identity r = Identity r
  type instance UnliftedCase Identity r = Identity r
  mkCaseable = id
  unCaseable = id
  matchCases a f = f a

testMaybe :: Maybe Int -> Bool
testMaybe a = runIdentity $ match (Identity a) \case -- it can be rebound to @case (Identity a) of@
  (Identity (Just _)) -> pure True
  (Identity Nothing)  -> pure False

----------------------------------------------------------------------------------------------------

-- * Here is the real use case for RebindableCase

newtype ExprPat (m :: Type -> Type) a = MkExprPat { unExprPat :: Expr (m a) }

class Calculatable m a where
  calc :: m a -> a

-- | A calculator for the toddlers who understand pattern matching.
data Expr a where
  -- ^ Embed any value types.
  Val :: forall a. a -> Expr a
  -- ^ Doing some toddler-level arithmetic for expressions.
  Add :: forall a. Expr a -> Expr a -> Expr a
  Mul :: forall a. Expr a -> Expr a -> Expr a
  Abs :: forall a. Expr a -> Expr a
  Sig :: forall a. Expr a -> Expr a
  Neg :: forall a. Expr a -> Expr a
  -- ^ Pattern matching.
  Pat :: forall a b m. (Caseable a (ExprPat m), Calculatable Expr (m a))
      => Expr (m a)
      -> (LiftedCase (ExprPat m) a -> UnliftedCase (ExprPat m) b)
      -> Expr (m b)

instance (Num a, Integral a, Bounded a) => Calculatable Expr (Maybe a) where
  calc (Val a)   = a
  calc (Add a b) = bounded_op2 (+) (calc a) (calc b)
  calc (Mul a b) = bounded_op2 (*) (calc a) (calc b)
  calc (Abs a)   = bounded_op1 abs (calc a)
  calc (Sig a)   = bounded_op1 signum (calc a)
  calc (Neg a)   = bounded_op1 negate (calc a)
  calc (Pat a f) = -- let a' = liftCase (MkExprPat (Val (calc a))) -- :: Maybe (Expr a')
    case calc a of
      Just a' -> calc (f (Just (Val a')))
      Nothing -> calc (f Nothing)

instance (Num a, Integral a, Bounded a) => Caseable a (ExprPat Maybe) where
  type instance LiftedCase (ExprPat Maybe) r = Maybe (Expr r)
  type instance UnliftedCase (ExprPat Maybe) r = Expr (Maybe r)
  mkCaseable = MkExprPat
  unCaseable = unExprPat
  matchCases (MkExprPat e) f = Pat e f

-- tests

add2 :: Expr (Maybe Int8) -> Expr (Maybe Int8) -> Expr (Maybe Int8)
add2 a b = a + b

add3 :: Expr (Maybe Int8) -> Expr (Maybe Int8) -> Expr (Maybe Int8) -> Expr (Maybe Int8)
add3 a b c = a + b + c

-- >>> test_add3 1 2 3
-- >>> test_add3 1 100 42
-- >>> test_add3 (-100) (-100) 100
-- Just 6
-- Nothing
-- Nothing

test_add3 :: Integer -> Integer -> Integer -> Maybe Int8
test_add3 a b c = calc (add3 (fromInteger a) (fromInteger b) (fromInteger c))

safe_add2 :: Expr (Maybe Int8) -> Expr (Maybe Int8) -> Expr (Maybe Int8)
safe_add2 a b = match (a + b) \case -- rebindable case => case (a + b) of
  (Just (Val c)) ->  (Val (Just c))
  _ -> Val (Just (-1))

test_safe_add2 :: Integer -> Integer -> Int8
test_safe_add2 a b = fromJust $ calc $ safe_add2 (fromInteger a) (fromInteger b)

test_long_expr :: Integer -> Integer -> Integer -> Int8
test_long_expr a b c = let a' = fromInteger a
                           b' = fromInteger b
                           c' = fromInteger c
                       in fromJust $ calc $ safe_add2 (add2 a' b') c'

-- >>> test_safe_add2 2 3
-- >>> test_safe_add2 100 100
-- >>> test_safe_add2 1000 0
-- >>> test_long_expr 1 2 100
-- >>> test_long_expr 100 10 20
-- 5
-- -1
-- -1
-- 103
-- -1


----------------------------------------------------------------------------------------------------

{- * Internal trite -}

bounded_op1 :: forall a. (Integral a, Bounded a)
             => (Integer -> Integer) -> Maybe a -> Maybe a
bounded_op1 op1 (Just a) = if c <= fromIntegral (maxBound @a) && c >= fromIntegral (minBound @a)
                      then Just (fromIntegral c)
                      else Nothing
  where c = op1 (toInteger a)
bounded_op1 _ _ = Nothing
bounded_op1_unsafe op1 a = fromJust (bounded_op1 op1 (Just a))

bounded_op2 :: forall a. (Integral a, Bounded a)
             => (Integer -> Integer -> Integer) -> Maybe a -> Maybe a -> Maybe a
bounded_op2 op2 (Just a) (Just b) = if c <= fromIntegral (maxBound @a) && c >= fromIntegral (minBound @a)
                      then Just (fromIntegral c)
                      else Nothing
  where c = toInteger a `op2` toInteger b
bounded_op2 _ _ _ = Nothing
bounded_op2_unsafe op2 a b = fromJust (bounded_op2 op2 (Just a) (Just b))

instance forall a. ( Num a, Integral a, Bounded a
                   ) => Num (Expr (Maybe a)) where
  (+) = Add
  (*) = Mul
  abs = Abs
  signum = Sig
  negate = Neg
  fromInteger a = if c <= fromIntegral (maxBound @a) && c >= fromIntegral (minBound @a)
                  then Val (Just (fromIntegral c))
                  else Val Nothing
    where c = fromInteger a :: Integer
