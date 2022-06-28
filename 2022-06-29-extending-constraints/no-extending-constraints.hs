class A a where

data AnyA = forall a. A a => MkAnyA a

data A1 = A1
instance A A1

data A2 = A2
instance A A2

someAs :: [AnyA]
someAs = [MkAnyA A1, MkAnyA A2, MkAnyA A1]

-- PROBLEM: How to extend "Show" instances to all A-s so that we can do "show a"
instance Show AnyA where show (MkAnyA a) = "Which A?"

main :: IO ()
main = do
  (putStrLn . show) someAs
