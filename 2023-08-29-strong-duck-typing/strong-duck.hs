{- cabal:
build-depends: base, constraints, text
default-language: GHC2021
-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}


-- Taking an OO example from https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/Object-oriented_programming

import           Data.Constraint (Dict)
import           Data.Kind       (Constraint, Type)
import           Data.Text       (Text)
import           GHC.Records     (HasField (..))

--------------------------------------------------------------------------------
-- Toy Object-Oriented Domain
--------------------------------------------------------------------------------

data PersonHood = MkPersonHood
    { personalPublicRecord   :: Text
    , personalPrivateHistory :: Text
    }

data Subject = Math
             | OtherStuff

newtype Paper = MkPaper Text

newtype Grade = MkGrade Int
-- use mkYear to create valid grade values.

newtype Year = MkYear Int
-- use mkYear to create valid year values.

--------------------------------------------------------------------------------
-- Interfaces
--------------------------------------------------------------------------------

-- What is a person?
type IPerson r = ( HasField "name" r String
                 , HasField "introduceSelf" r (r -> IO ())
                 )
-- A data type for a person.
data Person r = MkPerson { name          :: String     -- static function
                         , introduceSelf :: r -> IO () -- member function
                         }

data Student = MkStudent { _person :: Person Student
                         , year    :: Year
                         }

-- NOTE: I wish this could work
-- @@
-- instance HasField x (Person Student) a => HasField x Student a where
--   getField (MkStudent { _person = p }) = getField @x (getField @"_person" p)
-- @@
-- NOTE: ugly boilerplate that can be done via TH or something similar to above?
instance HasField "name" Student String where
  getField (MkStudent { _person = p}) = p.name
instance HasField "introduceSelf" Student (Student -> IO ()) where
  getField (MkStudent { _person = p}) = p.introduceSelf

data Professor = MkProfessor { _person :: Person Professor
                             , year    :: Year
                             , teaches :: [Subject]
                             }

dramaticEntrance :: IPerson p => p -> IO ()
dramaticEntrance p = print $ "GM Y'all, I am " ++ p.name ++ "!!"

-- This is duck-typing!
introduce :: IPerson r => r -> IO ()
introduce p = do
  print $ "Here is our next person, " <> p.name
  p.introduceSelf p

main = do
  let bob = MkStudent (MkPerson "Bob" dramaticEntrance) (MkYear 2023)
  introduce bob
  print "Bye!"

{-
Local Variables:
fill-column: 120
haskell-process-type: cabal-repl
haskell-process-load-or-reload-prompt: t
End:
-}
