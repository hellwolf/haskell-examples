{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import           Prelude hiding (fix, length, sum)

length :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs

lengthF :: ([a] -> Int) -> [a] -> Int
lengthF rec []     = 0
lengthF rec (_:xs) = 1 + rec xs

lengthF' = \rec -> \case
    []   -> 0
    _:xs -> 1 + rec xs

fix f = let x = f x in x

length' = fix lengthF

data MyList = MyNil | MyCons Int MyList
data MyListF a = MyNilF | MyConsF Int a

newtype Fix f = Fix { unFix :: f (Fix f) }

testList :: Fix MyListF
testList = Fix (MyConsF 1 (Fix (MyConsF 2 (Fix (MyConsF 3 (Fix MyNilF))))))

myOut :: MyList -> MyListF MyList
myOut MyNil         = MyNilF
myOut (MyCons i xs) = MyConsF i xs

myIn :: MyListF MyList -> MyList
myIn MyNilF         = MyNil
myIn (MyConsF i xs) = MyCons i xs

instance Functor MyListF where
    fmap f MyNilF        = MyNilF
    fmap f (MyConsF i a) = MyConsF i (f a)

mySumF :: MyListF Int -> Int
mySumF MyNilF           = 0
mySumF (MyConsF i rest) = i + rest

mySum :: MyList -> Int
mySum = mySumF . fmap mySum . myOut

myCata :: (MyListF a -> a) -> MyList -> a
myCata f = f . fmap (myCata f) . myOut

myLength = myCata $ \case
    MyNilF      -> 0
    MyConsF _ l -> 1 + l

myMax = myCata $ \case
    MyNilF      -> 0
    MyConsF x y -> max x y

myMin = myCata $ \case
    MyNilF      -> 0
    MyConsF x y -> min x y

myTestList = MyCons 2 (MyCons 1 (MyCons 3 MyNil))

pack :: a -> (Int -> a -> a) -> MyListF a -> a
pack b f MyNilF        = b
pack b f (MyConsF x y) = f x y

unpack :: (MyListF a -> a) -> (a, Int -> a -> a)
unpack f = (f MyNilF, \i a -> f (MyConsF i a))

class Functor f => Cata a f where
    out :: a -> f a

cata :: Cata a f => (f b -> b) -> a -> b
cata f = f . fmap (cata f) . out

instance Cata MyList MyListF where
    out = myOut

data ListF a b = Nil | Cons a b deriving Functor

instance Cata [a] (ListF a) where
    out []     = Nil
    out (x:xs) = Cons x xs

sum :: Num a => [a] -> a
sum = cata $ \case
    Nil       -> 0
    Cons x xs -> x + xs

data BinaryTree a = Node a (BinaryTree a) (BinaryTree a) | Leaf deriving (Show, Foldable)
data BinaryTreeF a b = NodeF a b b | LeafF deriving Functor

instance Cata (BinaryTree a) (BinaryTreeF a) where
    out (Node a l r) = NodeF a l r
    out Leaf         = LeafF

invert :: BinaryTree a -> BinaryTree a
invert = cata $ \case
    LeafF       -> Leaf
    NodeF a l r -> Node a r l

data MaybeF a b = NothingF | JustF a deriving Functor

instance Cata (Maybe a) (MaybeF a) where
    out Nothing  = NothingF
    out (Just x) = JustF x

getOrDefault :: a -> Maybe a -> a
getOrDefault d = cata $ \case
    NothingF -> d
    JustF a  -> a
