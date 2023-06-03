-- -*- fill-column: 100; -*-
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

----------------------------------------------------------------------------------------------------
-- CSV Decoder API Allowing Equivalence Between Type List and Type Tuples
----------------------------------------------------------------------------------------------------

module CSVDecoderAPI
  ( (:>)(..)
  , module CSVDecoderTypes
  , csvDecode
  ) where

import           Data.Kind       (Type)

import           CSVDecoderCore
import           CSVDecoderTypes


type family EQUIV_TO_TLIST a :: Type where
  EQUIV_TO_TLIST () = ()
  EQUIV_TO_TLIST (a :> as) = EQUIV_TO_TLIST a :> as
  EQUIV_TO_TLIST (Val a) = Val a
  EQUIV_TO_TLIST [a] = [EQUIV_TO_TLIST a]
  EQUIV_TO_TLIST (a, b) = EQUIV_TO_TLIST a :> EQUIV_TO_TLIST b :> ()
  EQUIV_TO_TLIST (a, b, c) = EQUIV_TO_TLIST a :> EQUIV_TO_TLIST b :> EQUIV_TO_TLIST c :> ()

-- | Main API for the decoder through type variable ~t~ and its equivalent hlist form.
csvDecode :: forall t a as. (TList a as, EQUIV_TO_TLIST t ~ (a :> as))
           => String -> (String, Maybe (a :> as))
csvDecode = csv_decode @a @as
