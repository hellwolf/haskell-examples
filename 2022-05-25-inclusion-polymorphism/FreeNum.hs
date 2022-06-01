{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
-- author: Olaf Klinke
-- in response to https://mail.haskell.org/pipermail/haskell-cafe/2022-May/135315.html
module FreeNum where

-- The initial object of the Num class
newtype FreeNum = FreeNum {
    runNum :: forall r.
        (Integer -> r) -> -- fromInteger
        (r -> r -> r) ->  -- (+)
        (r -> r -> r) ->  -- (*)
        (r -> r) ->       -- negate
        (r -> r) ->       -- abs
        (r -> r) ->       -- signum
        r
    }

-- initial morphism.
toNum :: Num a => FreeNum -> a
toNum x = runNum x fromInteger (+) (*) negate abs signum
-- Note: there is no inverse for toNum.
-- Unlike the existential type, we can not cast a Double to FreeNum.

-- The terminal object of the Num class
data TerminalNum where
    FromNum :: Num a => a -> TerminalNum -- ^ terminal morphism

-- * algebraic operations on the FreeNum type

freeInteger :: Integer -> FreeNum
freeInteger n = FreeNum (\fromInteger _ _ _ _ _ -> fromInteger n)

addFreeNum :: FreeNum -> FreeNum -> FreeNum
addFreeNum x y = FreeNum (\fromInteger' plus times negate' abs' sign -> plus
    (runNum x fromInteger' plus times negate' abs' sign)
    (runNum y fromInteger' plus times negate' abs' sign))

multFreeNum :: FreeNum -> FreeNum -> FreeNum
multFreeNum x y = FreeNum (\fromInteger' plus times negate' abs' sign -> times
    (runNum x fromInteger' plus times negate' abs' sign)
    (runNum y fromInteger' plus times negate' abs' sign))

negateFreeNum :: FreeNum -> FreeNum
negateFreeNum x = FreeNum (\fromInteger' plus times negate' abs' sign -> negate'
    (runNum x fromInteger' plus times negate' abs' sign))

absFreeNum :: FreeNum -> FreeNum
absFreeNum x = FreeNum (\fromInteger' plus times negate' abs' sign -> abs'
    (runNum x fromInteger' plus times negate' abs' sign))

signFreeNum :: FreeNum -> FreeNum
signFreeNum x = FreeNum (\fromInteger' plus times negate' abs' sign -> sign
    (runNum x fromInteger' plus times negate' abs' sign))

instance Num FreeNum where
    fromInteger = freeInteger
    (+) = addFreeNum
    (*) = multFreeNum
    negate = negateFreeNum
    abs = absFreeNum
    signum = signFreeNum

-- * Category-theoretical stuff.

data NumSignature num = FromInteger Integer
    | Plus num num
    | Mult num num
    | Negate num
    | Abs num
    | Signum num

-- Num class, defined differently.
-- The above works for all classes with a signature.
class NumSig num where
    operations :: NumSignature num -> num

-- FreeNum is the Church encoding of FreeNum'.
-- Both types are isomorphic.
newtype FreeNum' = FixNum {runFixNum :: NumSignature FreeNum'}
-- the isomorphism
fromChurch :: FreeNum -> FreeNum'
fromChurch x = runNum x fromInteger' plus times negate' abs' sign where
    fromInteger' = FixNum . FromInteger
    plus x y = FixNum (Plus x y)
    times x y = FixNum (Mult x y)
    negate' = FixNum . Negate
    abs' = FixNum . Abs
    sign = FixNum . Signum

main = do
  let a = fromChurch $ freeInteger 42
      b = fromChurch $ freeInteger 1
      -- c = fromChurch $ runNum 4.2
      x = Plus a b
      y = Plus a b
  (print . show . operations) $ Plus a b
