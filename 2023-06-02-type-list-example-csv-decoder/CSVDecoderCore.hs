-- -*- fill-column: 100; -*-

module CSVDecoderCore where

----------------------------------------------------------------------------------------------------
-- Simple CSV Decoder Framework Using Type List (TList)
----------------------------------------------------------------------------------------------------

data Val a = Val a

-- | Type constructor (:>) and data constructor pun for creating type level heterogeneous list.
data a :> b = a :> b
-- | Operator (:>) being right associative allows bracket-free syntax.
infixr :>

class TList a as where
  csv_decode :: String -> (String, Maybe (a :> as))

instance forall a a' as'. (TList a (), TList a' as') => TList a (a' :> as') where
  csv_decode s = case csv_decode @a @() s of
    (s', Nothing) -> (s', Nothing)
    (',':s', Just (a :> ())) -> case csv_decode @a' @as' s' of
      (s'', Just as) -> (s'', Just (a :> as))
      (s'', Nothing) -> (s'', Nothing)
    (s', _) -> (s', Nothing)

instance forall a as. (TList a as) => TList (a :> as) () where
  csv_decode ('(':s) = case csv_decode @a @as s of
    (s', Nothing)     -> (s', Nothing)
    (')':s', Just a') -> (s', Just (a' :> ()))
    (s', _)           -> (s', Nothing)
  csv_decode s = (s, Nothing)

instance TList () () where
  csv_decode s = (s, Nothing)
