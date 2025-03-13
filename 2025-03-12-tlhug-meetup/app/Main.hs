module Main where

import Data.Char (toUpper)
import Data.Functor ((<&>))

import MyLib qualified (someFunc)
-- import everything from the module:
-- import Fun.FunctorDemo
import Fun.FunctorDemo (test1)
import Fun.FunctorDemo qualified as FF

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc

  print $ test1 [1, 3 .. 10]
  print $ FF.test1 [100 .. 120]

  getLine <&> (\line -> toUpper <$> line) >>= print

  getLine <&> (toUpper <$>) >>= print

  getLine <&> (FF.safeDigitToInt <$>) >>= print
