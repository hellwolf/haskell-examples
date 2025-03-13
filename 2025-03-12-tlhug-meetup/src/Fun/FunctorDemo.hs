module Fun.FunctorDemo where

import Data.Char (ord)

test1 xs = fmap (+ 1) xs

safeDigitToInt :: Char -> Maybe Int
safeDigitToInt c
  | (fromIntegral dec::Word) <= 9 = Just dec
  | (fromIntegral hexl::Word) <= 5 = Just (hexl + 10)
  | (fromIntegral hexu::Word) <= 5 = Just (hexu + 10)
  | otherwise = Nothing
  where
    dec = ord c - ord '0'
    hexl = ord c - ord 'a'
    hexu = ord c - ord 'A'