{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

class Liquidity lq where
  isInsolvent :: lq -> Bool

class (Liquidity (LQ a)) => A a where
  type LQ a
  totalLiquidity :: a -> LQ a
  isTotallyInsolvent :: a -> Bool
  isTotallyInsolvent = isInsolvent . totalLiquidity

instance (Num n, Ord n) => Liquidity n where
  isInsolvent = flip (<) 0

instance A [Int] where
  type LQ [Int] = Int
  totalLiquidity a = foldr (+) 0 a

instance A [Double] where
  type LQ [Double] = Double
  totalLiquidity a = foldr (+) 0 a

main :: IO ()
main = do
  let a = [1::Int, 2::Int, -4::Int]
  putStrLn $ show $ isInsolvent $ totalLiquidity a
  let b = [5.2::Double, 2.2::Double, -4.4::Double]
  putStrLn $ show $ isInsolvent $ totalLiquidity b
