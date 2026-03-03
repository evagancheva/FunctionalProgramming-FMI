module Sol7 where
import Data.Sequence (Seq())
import System.Win32 (xBUTTON1)
import Prelude hiding (lookup)

--task 1
-- Да се дефинира алгебричен тип данни, представящ естествените 
--числа в Пеановата аритметика. Да се дефинират следните функции:

data Nat = Zero | Succ Nat
    deriving(Show)

succ:: Nat -> Nat
succ = Succ
--succ n = Succ n

pred :: Nat -> Nat
pred Zero = Zero
pred (Succ n) = n

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = add m (Succ n)
--Succ $ add m n 

mult:: Nat -> Nat ->Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult (Succ m) n = add n (mult m n)

fromInt :: Int -> Nat
fromInt 0 = Zero
fromInt n = Succ $ fromInt (n-1)

toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ n) = 1 + toInt n

cmp :: Nat -> Nat -> Ordering
cmp Zero Zero = EQ
cmp Zero _ = LT
cmp _ Zero = GT
cmp (Succ n) (Succ m) = cmp n m

-- task 2
-- Да се дефинира алгебричен тип данни, представящ списък от елементи 
-- от произволен тип. Да се дефинират следните функции:

data List a = Nil | Cons a (List a)
    deriving (Show)

isEmpty :: List a -> Bool
isEmpty Nil = True
isEmpty _ = False

headList :: List a -> Maybe a
headList Nil = Nothing
headList (Cons x _) = Just x

singleton :: a -> List a
singleton x = Cons x Nil

(+++) :: List a -> List a -> List a
Nil +++ l2 = l2
(Cons x xs) +++ l2 = Cons x $ xs +++ l2

reverseList :: List a -> List a
reverseList Nil = Nil
reverseList (Cons x xs) = reverseList xs +++ singleton x

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x $ fromList xs
-- foldr Cons Nil (x:xs)

toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

mapList :: (a -> b) -> List a -> List b
mapList _ Nil = Nil
mapList f (Cons x xs) = Cons (f x) $ mapList f xs

intersperse :: a -> List a -> List a
intersperse _ Nil = Nil
intersperse _ (Cons x xs) = singleton x
intersperse a (Cons x xs) = Cons x $ Cons a $ intersperse a xs

-- task 3
-- Нека е дадена следният индуктивен алгебричен тип данни, представящ
-- израз, който се оценява до стойност от числен тип:

data Expr a
  = Constant a
  | Variable String
  | Expr a :+: Expr a
  | Expr a :*: Expr a
  deriving (Show, Eq, Ord)

newtype Dict k v = Dict [(k, v)]
  deriving (Show)

-- Използвайки ваш АТД за речник, дефинирайте функция, която оценява такъв израз, 
-- като оценката на променлива var се замества със стойността value на двойката ключ-стойност 
-- (var, value) в речника. Ако такава няма, то няма как изразът да бъде оценен.

lookup::(Eq k) => k -> Dict k v -> Maybe v
lookup _ (Dict []) = Nothing
lookup key (Dict ((k,v) : kvps)) = if key == k then Just v else lookup key (Dict kvps)

eval :: Num a => Dict String a -> Expr a -> Maybe a
eval _ (Constant c) = Just c
eval dict (Variable x) = lookup x dict
eval dict (lhs :+: rhs) =
    let lhs1 = eval dict lhs
        rhs1 = eval dict rhs
    in case (lhs1,rhs1) of
        (Just val1, Just val2) -> Just (val1 + val2)
        _ -> Nothing
eval dict (lhs :*: rhs) =
    let lhs1 = eval dict lhs
        rhs1 = eval dict rhs
    in case (lhs1,rhs1) of
        (Just val1, Just val2) -> Just (val1 * val2)
        _ -> Nothing

dict :: Dict String Int
dict = Dict [("x", 1), ("y", 2)]

expr1 :: Expr Int
expr1 = Variable "x" :+: (Variable "y" :*: Constant 3)

expr2 :: Expr Int
expr2 = Constant 2 :*: Variable "z"

-- >>> eval dict expr1
-- Just 7

-- >>> eval dict expr2
-- Nothing

-- --task 4
-- -- Да се дефинира алгебричен тип данни, представящ дърво с произволен брой
-- -- наследници. Да се дефинират следните функции:


data Tree a = Node a [Tree a]
    deriving (Show)

countNodes :: Tree a -> Int
countNodes (Node _ []) = 1
countNodes (Node _ xs) = 1+ sum (map countNodes xs)

countLeaves :: Tree a -> Int
countLeaves (Node _ []) = 1
countLeaves (Node _ xs) = sum (map countNodes xs)

contains :: Eq a => a -> Tree a -> Bool
contains x (Node y ys) = x == y || any ( contains x) ys

flatten :: Tree a -> [a]
flatten (Node a xs) = a : concatMap flatten xs

-- Задача 05
-- Да се дефинира алгебричен тип данни, представящ двоично дърво с 
-- елементи от произволен тип. Да се дефинират следните функции:

data BinTree a = TNode a (BinTree a ) (BinTree a) | Empty

countLeavesBT :: BinTree a -> Int
countLeavesBT Empty = 0
countLeavesBT (TNode _ Empty Empty) = 1
countLeavesBT (TNode x l r) = countLeavesBT l + countLeavesBT r

height :: BinTree a -> Int
height Empty = 0
height (TNode _ Empty Empty) = 1
height (TNode x l r) = 1 + max (height l) (height r)

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT _ Empty = Empty
mapBT f (TNode x l r) = TNode (f x) (mapBT f l) (mapBT f r)

inorder :: BinTree a -> [a]
inorder Empty = []
inorder (TNode x l r) = inorder l ++ [x] ++ inorder r

preorder :: BinTree a -> [a]
preorder Empty =[]
preorder (TNode x l r) = [x] ++ preorder l ++ preorder r

--toBST :: Ord a => [a] -> BinTree a

-- isBalancedBST :: (Ord a, Bounded a) => BinTree a -> Bool.
-- Задача 06 - контролно по ФП на КН2, 2024/2025г.
-- Да се дефинира функция, която приема двоично дърво и връща максималната 
-- дължина на път в дървото, в който произведението на нечетните елементи е минимално
