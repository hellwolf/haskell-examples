#!/usr/bin/env -S cabal run -v1
{- cabal:
build-depends: base, template-haskell, constraints
default-language: GHC2024
ghc-options: -Wall
-}
-- Synopsis: Correct way to capture runtime value of a SNat for constraints based on Nat value
-- Discourse thread: https://discourse.haskell.org/t/capture-type-level-nat-with-withknownnat/11080/8
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell     #-}
-- base
import GHC.TypeLits
import Unsafe.Coerce       (unsafeCoerce)
-- template-haskell
import Language.Haskell.TH qualified as TH
-- constraints
import Data.Constraint     (Dict (Dict), (\\))

class KnownNat n => ValidUINTn n

-- | This represents the type information of an unsigned integer  with n-bytes.
data UINTn where
  UINTn :: ValidUINTn n => SNat n -> UINTn

-- ^ Show the fashionable "U8", "U16", ... "U256".
instance Show UINTn where
  show (UINTn sn) = "U" <> show (natVal sn * 8)

-- | This top-level splice generates all 32 of 'ValidUINTn' instances.
flip foldMap [1..32] $ \i -> [d| instance ValidUINTn $(TH.litT (TH.numTyLit i)) |]

-- | Maybe create a valid 'UINTn' from a runtime integer.
getValidUINTn :: Integer -> Maybe UINTn
getValidUINTn x =
  withSomeSNat x $ \maybeSn -> maybeSn >>=
  \sn -> let n = fromSNat sn
             -- this minimizes the need of template haskell code
             g :: forall n. ValidUINTn n => Maybe UINTn
             g = Just (UINTn (natSing @n))
            -- create all 32 cases using template-haskell:
            -- case n of 1 -> g @1; 2 -> g @2; ...; _ -> _;
         in $(TH.caseE
              (TH.varE 'n)
              ( map
                (\i -> TH.match
                       (TH.litP (TH.integerL i))
                       (TH.normalB (TH.varE 'g `TH.appTypeE` TH.litT (TH.numTyLit i)))
                       [])
                [1..32]
                ++ [ TH.match TH.wildP (TH.normalB (TH.conE 'Nothing)) [] ]
              )
            )

-- | Maybe get a ValidUINTn from a runtime SNat value.
unsafeValidUINTn :: forall n. KnownNat n => SNat n -> Maybe (Dict (ValidUINTn n))
unsafeValidUINTn sn = let n = fromSNat sn
                      in if n >= 1 && n <= 32
                         then Just (unsafeCoerce (Dict @(KnownNat n)))
                         else Nothing

-- | A variant of getValidUINTn using 'unsafeValidUINTn'.
getValidUINTn' :: Integer -> Maybe UINTn
getValidUINTn' x =
  withSomeSNat x $ \maybeSn -> maybeSn >>=
  \sn -> withKnownNat sn (unsafeValidUINTn sn) >>= (Just (UINTn sn) \\)

main :: IO ()
main = do
  print $ getValidUINTn 4
  print $ getValidUINTn 32
  print $ getValidUINTn 33
  -- use unsafeValidUINTn
  print $ getValidUINTn' 8

{-
Local Variables:
fill-column: 100
haskell-process-type: cabal-repl
haskell-process-load-or-reload-prompt: t
End:
-}
