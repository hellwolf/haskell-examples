#!/usr/bin/env -S cabal run -v1
{- cabal:
build-depends: base
ghc-options: -Wall
-}
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

import Prelude -- RebindableSyntax excludes it
import GHC.TypeLits
import Data.Type.Bool
import GHC.TypeError                    (Assert, Unsatisfiable)
import GHC.Records
import Data.Kind (Type)

infixr 5 :*

data NamedField (n :: Symbol) (a :: Type) = MkNamedField a
  deriving Show

type family FieldNameFound (xs :: [Type]) (n :: Symbol) :: Bool where
  FieldNameFound (NamedField n  _ : _) n = True
  FieldNameFound (NamedField _  _ : _) n = False
  FieldNameFound '[]                   _ = False

data TypedFields :: [Type] -> Type where
  Nil  :: TypedFields '[]
  (:*) ::
    ( Show a,
      Assert (Not (FieldNameFound xs n)) (Unsatisfiable (Text "Duplicate filed name found"))
    ) =>
    NamedField n a -> TypedFields xs -> TypedFields (NamedField n a : xs)

deriving instance Show (TypedFields xs)

type TypedRecord xs = TypedFields xs

type SocialLinks = TypedRecord
  '[ NamedField "github" String
   ]

type Person = TypedRecord
  [ NamedField "name" String
  , NamedField "age" Int
  -- , NamedField "age" Double -- duplicate field detection working
  , NamedField "friends" [Person'] -- NOTE! recursive definition of type alias not allowed
  , NamedField "social" SocialLinks
  ]
newtype Person' = MkPerson Person

instance Show Person' where
  show (MkPerson (MkNamedField name :*
                  MkNamedField age :*
                  MkNamedField friends :*
                  MkNamedField social :*
                  Nil)) =
    "name: " <> name <> ", " <>
    "age: " <> show age <> ", " <>
    "friends: " <> show (fmap (\(MkPerson p) -> p.name) friends) <> ", " <>
    "social: " <> show social

mkPerson :: String -> Int -> [Person] -> String -> Person
mkPerson name age friends githubAccount =
  ( MkNamedField name :*
    MkNamedField age :*
    MkNamedField (fmap MkPerson friends) :*
    MkNamedField (MkNamedField githubAccount :* Nil) :*
    Nil)

instance HasField n (TypedFields (NamedField n a:xs)) a where
  getField (MkNamedField x :* _) = x

instance {-# OVERLAPPABLE #-} HasField n (TypedFields xs) a => HasField n (TypedFields (x:xs)) a where
  getField (_ :* xs) = getField @n xs

main = do
  let alice = mkPerson "alice" 32 [] "alice.eth"
  let bob = mkPerson "bob" 42 [alice] "bob.sol"
  print bob
  print (MkPerson bob) -- different show instance
  print (bob.name, bob.age)
  print bob.friends
  print bob.social.github

{-
Local Variables:
fill-column: 100
haskell-process-type: cabal-repl
haskell-process-load-or-reload-prompt: t
End:
-}
