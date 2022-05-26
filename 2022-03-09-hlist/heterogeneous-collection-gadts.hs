{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

import           Data.Typeable

--
-- Showable Type Class
--
class (Show a, Typeable a) => Showable a where
  findShowable :: [AnyShowable] -> proxy a -> [AnyShowable]
  findShowable alist p = foldr (++) [] $ map
    (\(MkShowable x) -> if typeOf x == typeRep p
      then [MkShowable x]
      else []
    ) alist

data AnyShowable where
  MkShowable :: (Showable a) => a -> AnyShowable
-- Alternative Syntax:
-- data AnyShowable = forall a . Showable a => MkShowable a

instance Show AnyShowable where
  show (MkShowable a) = "«"++(show $ typeOf a)++"» "++(show a)

---
--- R Type
---
data R = R
  { n :: String
  , v :: Int }

instance Show R where
  show R{..} = n++": "++(show v)

isR :: AnyShowable -> Bool
isR (MkShowable x) = (typeOf x) == (typeRep (Proxy @R))

--
-- Generate All Showables
--
instance (Show a, Typeable a) => Showable a where


-- exampleVal :: Proxy a -> a
-- exampleVal (_ :: Proxy Int) = 42

main = do
  let a = R "a" 42
  let coll =
        [ MkShowable (R "a" 42)
        , MkShowable "bob"
        , MkShowable (1::Int)
        , MkShowable (2::Int)]
  putStr "The collection: ["
  mapM_ (putStr . (++ ", ") . show) coll
  putStrLn "]\n"
  putStr "The collection mapped isR: ["
  mapM_ (putStr . (++ ", ") . show . isR) coll
  putStrLn "]\n"
  putStrLn "Searching R..."
  putStrLn $ show $ findShowable coll (Proxy @R)
  putStrLn ""
  putStrLn "Searching Int..."
  putStrLn $ show $ findShowable coll (Proxy @Int)
