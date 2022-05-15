{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Sized
    ( Sized (..)
    , empty
    , length
    , pattern (:<)
    , pattern (:>)
    , pattern Nil
    , toList
    , unsafeFromList'
    , (|>)
    )
import           GHC.TypeNats (KnownNat)

type AddressList n = Sized [] n String
type AddressLengthList n = Sized [] n Int

data Spec n = Spec
  { addresses :: AddressList n
  }

getAddressLengths :: (KnownNat n) => Spec n -> AddressLengthList n
getAddressLengths = unsafeFromList' . (Prelude.map Prelude.length) . toList . addresses

-- s1 :: Spec n
-- s1 = Spec (unsafeFromList' ["bob", "alice", "carol"])

-- s2 :: (KnownNat n) => Spec n
s2 = Spec (empty |> "bob dylan" |> "alice wandlerloo" |> "carol jasmine")

main :: IO ()
main = do
  print $ addresses s2
  print $ show $ Data.Sized.length $ (addresses s2)
  print $ show $ getAddressLengths $ s2
  -- cons pattern matching
  let (a :< _) = getAddressLengths $ s2
  putStrLn . show $ (a)
  let (a :< _ :< _ :< Nil) = getAddressLengths $ s2
  putStrLn . show $ (a)
  -- snoc pattern matching
  let (_ :> a) = getAddressLengths $ s2
  putStrLn . show $ a
  -- how to break it down??
  let (Nil :> (_ :: Int) :> (_ :: Int) :> (_ :: Int)) = getAddressLengths $ s2
  -- putStrLn . show $ a
  print "END."
