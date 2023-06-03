-- -*- fill-column: 100; -*-

module CSVDecoderTypes where

----------------------------------------------------------------------------------------------------
-- Supported Primitive Types for the CSVDecoder
----------------------------------------------------------------------------------------------------

import           Data.Monoid    (Last (..))

import           CSVDecoderCore


type TNil = () :> ()
type OneVal a = a :> ()

type IntVal = Val Int
instance TList IntVal () where
  csv_decode s = case reads s :: [(Int, String)] of
    [(x, s')] -> (s', Just (Val x :> ()))
    _         -> (s, Nothing)


type StringVal = Val String
instance TList StringVal () where
  csv_decode s = case reads s :: [(String, String)] of
    [(x, s')] -> (s', Just (Val x :> ()))
    _         -> (s, Nothing)

instance TList a () => TList [a] () where
  csv_decode ('[':s) = case go s of
                         (Last (Just s'), Right Nothing, xs) -> (s', Just (xs :> ()))
                         (Last (Just s'), Left (), _)        -> (s', Nothing)
    where go s = case csv_decode @a @() s of
            (',':s', Just (x :> ())) -> ((Last (Just s'), Right Nothing, [x]) <> go s')
            (']':s', Just (x :> ())) -> ((Last (Just s'), Right Nothing, [x]))
            (s', Nothing)            -> (Last (Just s'), Left (), [])
  csv_decode s = (s, Nothing)

instance Show a => Show (Val a) where
  show (Val a) = show a

instance {-# OVERLAPPABLE #-} (Show a, Show b) => Show (a :> b) where
  show (a :> b) = show a <> "," <> show b

instance {-# OVERLAPPING  #-} (Show a, Show b) => Show (Val a :> b) where
  show (a :> b) = "(" <> show a <> "," <> show b <> ")"

instance {-# OVERLAPPING  #-} Show a => Show (Val a :> ()) where
  show (a :> b) = show a
