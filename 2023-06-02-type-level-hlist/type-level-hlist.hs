-- -*- fill-column: 100; -*-
{-# LANGUAGE TypeFamilies #-}

import           Data.Kind   (Type)
import           Data.Monoid (Last (..))
import           Data.Proxy  (Proxy (..))

----------------------------------------------------------------------------------------------------
-- Simple String Decoder Framework Using Type Level Heterogeneous List (HList)
----------------------------------------------------------------------------------------------------

data Val a = Val a deriving Show

type IntVal = Val Int
type StringVal = Val String

-- | Type constructor (:>) and data constructor pun for creating type level heterogeneous list.
data a :> b = a :> b
-- | Operator (:>) being right associative allows bracket-free syntax.
infixr :>

type TNil = () :> ()
type OneVal a = a :> ()

class TList a as where
  abi_decode :: String -> (String, Maybe (a :> as))

instance forall a a' as'. (TList a (), TList a' as') => TList a (a' :> as') where
  abi_decode s = case abi_decode @a @() s of
    (s', Nothing) -> (s', Nothing)
    (',':s', Just (a :> ())) -> case abi_decode @a' @as' s' of
      (s'', Just as) -> (s'', Just (a :> as))
      (s'', Nothing) -> (s'', Nothing)
    (s', _) -> (s', Nothing)

instance forall a as. (TList a as) => TList (a :> as) () where
  abi_decode ('(':s) = case abi_decode @a @as s of
    (s', Nothing)     -> (s', Nothing)
    (')':s', Just a') -> (s', Just (a' :> ()))
    (s', _)           -> (s', Nothing)
  abi_decode s = (s, Nothing)

instance TList () () where
  abi_decode s = (s, Nothing)

-- | Main API for the decoder through type variable ~t~ and its equality to the hlist form.
--
--   * Example usage: ~abiDecode @() s~
abiDecode :: forall t a as. (TList a as, t ~ (a :> as))
          => String -> (String, Maybe (a :> as))
abiDecode = abi_decode @a @as

----------------------------------------------------------------------------------------------------
-- Supported Primitive Types
----------------------------------------------------------------------------------------------------

instance TList IntVal () where
  abi_decode s = case reads s :: [(Int, String)] of
    [(x, s')] -> (s', Just (Val x :> ()))
    _         -> (s, Nothing)

instance TList StringVal () where
  abi_decode s = case reads s :: [(String, String)] of
    [(x, s')] -> (s', Just (Val x :> ()))
    _         -> (s, Nothing)

instance TList a () => TList [a] () where
  abi_decode ('[':s) = case go s of
                         (Last (Just s'), Last Nothing, Just xs) -> (s', Just (xs :> ()))
                         (Last (Just s'), _, _)                  -> (s', Nothing)
    where go s = case abi_decode @a @() s of
            (',':s', Just (x :> ())) -> ((Last (Just s'), Last Nothing, Just [x]) <> go s')
            (']':s', Just (x :> ())) -> ((Last (Just s'), Last Nothing, Just [x]))
            (s', Nothing)            -> (Last (Just s'), Last (Just ()), Nothing)
  abi_decode s = (s, Nothing)

deriving instance (Show a, Show b) => Show (a :> b)

----------------------------------------------------------------------------------------------------
-- Syntactic Sugar Through Establishing Equivalence Between Type Level HList & Tuples
----------------------------------------------------------------------------------------------------

type family TUPLE_TO_TLIST a :: Type where
  TUPLE_TO_TLIST (OneVal a) = TUPLE_TO_TLIST a :> () -- FIXME is there way to avoid such workaround?
  TUPLE_TO_TLIST (Val a) = Val a
  TUPLE_TO_TLIST [a] = [TUPLE_TO_TLIST a]
  TUPLE_TO_TLIST (a, b) = TUPLE_TO_TLIST a :> TUPLE_TO_TLIST b :> ()
  TUPLE_TO_TLIST (a, b, c) = TUPLE_TO_TLIST a :> TUPLE_TO_TLIST b :> TUPLE_TO_TLIST c :> ()

abiDecode' :: forall t a as. (TList a as, TUPLE_TO_TLIST t ~ (a :> as))
           => Proxy t -> String -> (String, Maybe (a :> as))
abiDecode' _ = abi_decode @a @as

----------------------------------------------------------------------------------------------------
-- Test Code
----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  -- Use "Sugarfree" HList Syntax
  print $ abiDecode @TNil ""
  print $ abiDecode @(IntVal :> ())
    "42"
  print $ abiDecode @(IntVal :> StringVal :> IntVal :> ())
    "42,\"france\",41"
  print $ abiDecode @(IntVal :> (StringVal :> IntVal :> ()) :> ())
    "42,(\"estonia\",14)"
  print $ abiDecode @(IntVal :> (StringVal :> IntVal :> ()) :> StringVal :> ())
    "42,(\"estonia\",14),\"finland\""
  print $ abiDecode @(IntVal :> (StringVal :> IntVal :> ()) :> (StringVal :> IntVal :> ()) :> ())
    "42,(\"estonia\",14),(\"finland\",50)"
  -- Use Tuple Syntax
  print $ abiDecode' (Proxy @(OneVal IntVal))
    "42"
  print $ abiDecode' (Proxy @(IntVal, (StringVal, IntVal)))
    "42,(\"france\",41)"
  print $ abiDecode' (Proxy @(StringVal, [IntVal]))
    "\"germany\",[1,3,5,7,11]"
  print $ abiDecode' (Proxy @(StringVal, [(IntVal, IntVal)]))
    "\"germany\",[(1,3),(5,7),(11,13)]"
  print $ abiDecode' (Proxy @(StringVal, [[IntVal]]))
    "\"germany\",[[1],[3,5,7]]"
  -- Use patter matching to consume the parsed values
  case abiDecode @(IntVal :> IntVal :> ()) "4,2" of
    (_, Just (a :> b :> ())) -> print (a, b)
    _                        -> error "not possible"
