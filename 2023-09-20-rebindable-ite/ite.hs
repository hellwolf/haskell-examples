{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RebindableSyntax #-}

import           Prelude

class IfThenElse a b where
    ifThenElse :: a -> b -> b -> b

-- For some weird eDSL
data Expr a where
  Lit      :: a -> Expr a
  Add      :: Expr Int  -> Expr Int -> Expr Int
  LessThan :: Expr Int  -> Expr Int -> Expr Bool
  ITE      :: Expr Bool -> Expr a -> Expr a -> Expr a

deriving instance Show a => Show (Expr a)

instance IfThenElse (Expr Bool) (Expr a) where
    ifThenElse c a b = ITE c a b

f :: Int -> Int -> String
f a b = show $
  if LessThan (Add (Lit a) (Lit b)) (Lit 100) then Lit "Not enough" else Lit "Thank you"

-- Compatible with built-in Bool

instance IfThenElse Bool a where
    ifThenElse True  a _ = a
    ifThenElse False _ b = b

g a b = if a + b < 100 then "Not enough" else "Thank you"
