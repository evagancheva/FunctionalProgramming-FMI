
module Sol3 where
import Prelude hiding(repeat,Ordering, Maybe, Nothing, Just)
--task 1
-- Да се дефинира функция, която приема елемент от произволен тип и връща безкраен списък, 
--съдържащ този елемент:

repeat :: a -> [a]
repeat x = x : repeat x

-- >>> take 5 (repeat "hi")
-- ["hi","hi","hi","hi","hi"]

--task 2
-- Да се дефинира функция, приемаща цяло число n и генерираща безкраен списък [n, n+1, ..]. 
--За целта не използвайте генератор на списъци.

from ::Int ->[Int]
from n = n : from (succ n)

-- >>> take 10 (from 4)
-- [4,5,6,7,8,9,10,11,12,13]

--task 3
-- Да се дефинира безкраен списък от числата на Фибоначи.

fibs :: [Int]
fibs = helper 0 1
    where
        helper:: Int -> Int -> [Int]
        helper a b = a : helper b (a+b)
-- >>> take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]

--task 4 
-- Да се дефинира алгебричен тип данни, представящ релации на наредба между елементи,
-- като за два елемента x и y е вярно, че:
-- или x < y;
-- или x == y;
-- или x > y.
-- Да се дефинира функция, която сравнява два елемента от целочислен тип и връща 
--стойност от горния АТД.

data Ordering = Less | Equal | Greater
    deriving(Show, Eq,Ord,Enum)
cmpInt :: Int -> Int -> Ordering
cmpInt a b
    | a<b =Less
    | a == b =Equal
    | a >b =Greater
-- >>> cmpInt 7 1
-- Greater

--task 5 
-- Да се дефинира алгебричен тип данни, представящ фигура в равнината, която може да
-- бъде триъгълник, квадрат или правилен многоъгълник, представена чрез броя страни, 
-- които има, и дължините на тези страни. Да се дефинират следните функции:
-- perimeter :: Shape -> Double;
-- numberOfSides :: Shape -> Int;
-- prettyPrint :: Shape -> String, където изходът трябва да е в следния формат:

type Side = Int
type Length = Double

data Shape = Triangle Length Length Length
            | Square Length
            | Polygon Side Length
            deriving (Show,Eq,Ord)

perimeter:: Shape -> Double
perimeter (Triangle a b c ) = a +b+c
perimeter (Square a ) =  a*4
perimeter (Polygon cnt a ) = fromIntegral cnt * a

-- >>> perimeter (Polygon 4 3)
-- 12.0
numberOfSides::Shape ->Int
numberOfSides (Triangle{}) = 3
numberOfSides (Square _) = 4
numberOfSides (Polygon cnt _) = cnt
-- >>> numberOfSides (Square 4)
-- 4

prettyPrint:: Shape ->String
prettyPrint s =
  "This figure is a "
    ++ case s of
      Square x -> "square with a side of length " ++ show x
      Triangle x y z -> "triangle with sides " ++ show x ++ ", " ++ show y ++ ", and " ++ show z
      Polygon n x -> "regular polygon that has " ++ show n ++ " sides, each with a length of " ++ show x

-- case sintaksis
-- >>>prettyPrint (Polygon 5 6.6) 
-- "This figure is a regular polygon that has 5 sides, each of length 6.6"

--task 6
-- Да се дефинира алгебричен тип данни, представящ наредена двойка, където двете 
--компоненти могат да бъдат от произволен тип. Да се дефинират следните функции:
-- myFst :: Pair a b -> a;
-- mySnd :: Pair a b -> b;
-- myRev :: Pair a b -> Pair b a;
-- pairToTuple :: Pair a b -> (a,b);
-- tupleToPair :: (a,b) -> Pair a b;
-- cmpPair :: (Ord a, Ord b) => Pair a b -> Pair a b -> Ordering, където Ordering е АТД от задача 04;
-- pairsToList :: [Pair a b] -> Pair [a] [b].

data Pair a b = Pair a b
    deriving(Show)

myFst :: Pair a b -> a
myFst (Pair a b) = a

mySnd :: Pair a b -> b
mySnd (Pair a b ) = b

myRev :: Pair b a -> Pair a b
myRev (Pair a b) = (Pair b a )
-- >>> myRev (Pair 3 4)
-- Pair 4 3

pairToTuple :: Pair a b -> (a, b)
pairToTuple (Pair a b) =(a,b)
-- >>> pairToTuple(Pair 3 4 )
-- (3,4)

tupleToPair :: (a, b) -> Pair a b
tupleToPair (a,b) = Pair a b
-- >>> tupleToPair (3,4)
-- Pair 3 4
cmp :: (Ord a) =>a ->a ->Ordering
cmp a b
    | a<b =Less
    | a == b =Equal
    | a >b =Greater

cmpPair :: (Ord a, Ord b) => Pair a b -> Pair a b -> Ordering
cmpPair (Pair x1 y1) (Pair x2 y2)
    |cmp x1 x2 == Equal = cmp y1 y2
    | otherwise =cmp x1 x2
-- >>>cmpPair (Pair 1 4) (Pair 3 4)
-- Less

pairsToList :: [Pair a b] -> Pair [a] [b]
pairsToList pairs = helper [] [] pairs
  where
    helper :: [a] -> [b] -> [Pair a b] -> Pair [a] [b]
    helper as bs [] = Pair as bs
    helper as bs ((Pair a b) : xs) =
      helper (as ++ [a]) (bs ++ [b]) xs

-- >>> pairsToList [Pair 1 2, Pair 3 4]
-- Pair [1,3] [2,4]

--task 7
-- Да се дефинира алгебричен тип данни Maybe, аналогичен на този, за който сте говорили
-- на лекции. Да се дефинират следните функции:
-- safeDiv :: Double -> Double -> Maybe Double;
-- addM :: Maybe Int -> Maybe Int -> Maybe Int;
-- sumM :: [Maybe Int] -> Maybe Int;
-- isJust :: Maybe a -> Bool;
-- isNothing :: Maybe a -> Bool;
-- fromJust :: Maybe a -> a.

data Maybe a = Just a | Nothing
    deriving (Show)

safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv a b = Just (a/b)

-- >>> safeDiv 5 6
-- Just 0.8333333333333334
-- >>> safeDiv 9 0
-- Nothing

addM :: Maybe Int -> Maybe Int -> Maybe Int
addM (Just a) (Just b) = Just (a+b)
addM _ _ = Nothing
-- >>> addM (Just 1) (Just 6)
-- Just 7
-- >>> addM Nothing (Just 8)
-- Nothing

sumM :: [Maybe Int] -> Maybe Int
sumM [] = Just 0
sumM (Nothing : _) = Nothing
sumM ((Just x):xs) = case sumM xs of
    Just s -> Just(x+s)
    _ -> Nothing

-- >>> sumM (take 10 (repeat Nothing))
-- Nothing

-- >>> sumM []
-- Just 0
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing =True
isNothing _ = False

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust _ = error "no value of type a in Nothing"

sort' :: Ord a => [a] -> [a]
sort' [] = []
sort' (x:xs) = insert x (sort' xs)
  where
    insert y [] = [y]
    insert y (z:zs)
      | y <= z    = y : z : zs
      | otherwise = z : insert y zs

--task 8
-- Да се дефинира алгебричен тип данни, представящ множество от елементи от произволен тип.
-- Да се дефинират следните функции:
-- fromList :: [a] -> Set a;
-- toList :: Set a -> [a];
-- insert :: a -> Set a -> Set a;
-- delete :: a -> Set a -> Set a;
-- elemS :: a -> Set a -> Bool;
-- union :: Set a -> Set a -> Set a;
-- intersect :: Set a -> Set a -> Set a;
-- equal :: Set a -> Set a -> Bool.
-- Забележка: Някои от типовете на горните функции са непълни. Допълнете ги по такъв начин, че да се компилират и работят коректно.

newtype Set a = Set [a]
    deriving (Show)

nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x : xs)
  | x `elem` xs = nub xs
  | otherwise = x : nub xs

fromList :: (Ord a ) =>[a] -> Set a
fromList xs = Set(nub(sort' xs))

toList :: Set a -> [a]
toList (Set a) = a

insert :: (Ord a )=>a -> Set a -> Set a
insert a (Set s) = Set(helper a s)
    where
        helper :: (Ord a) => a ->[a] ->[a]
        helper a [] = [a]
        helper a l@(y:ys) = 
            case cmp a y of
                Equal -> l
                Less -> a:y:ys
                Greater -> y : helper a ys


delete :: (Ord a) => a -> Set a -> Set a
delete a (Set s) = Set(helper a s )
    where
        helper :: (Ord a) => a ->[a] ->[a]
        helper a [] = [a]
        helper a (s:xs) 
            | a == s = xs 
            | otherwise = s:helper a xs

elemS ::(Eq a)=> a -> Set a -> Bool
elemS a (Set s) = helper a s
    where
        helper::(Eq a) => a -> [a] ->Bool
        helper a [] = False
        helper a (x:s) 
            | a == x = True
            | otherwise = helper a s 

set1 :: Set Int
set1 = fromList [1, 1, 5, 1, 2, 0, 10, 4, 6]

union :: (Ord a) => Set a -> Set a -> Set a
union (Set s1) (Set s2) = Set(merge s1 s2)
    where
        merge:: (Ord a) => [a] -> [a] -> [a]
        merge [] l2 = l2
        merge l1 [] = l1 
        merge l1@(x:xs) l2@(y:ys)
            | x == y =merge xs l2
            | x< y = x:merge xs l2
            | x>y = y:merge l1 ys

set2 :: Set Int
set2 = fromList [9, 1, 5, 3, 2, 4, 8]

intersect :: (Ord a) => Set a -> Set a -> Set a
intersect (Set xs) (Set ys) = Set(helper xs ys)
    where
        helper [] _ = []
        helper _ [] = []
        helper l1@(x:xs) l2@(y:ys) 
            | x == y = x : helper xs ys
            | x < y = helper xs l2
            | x > y = helper l1 ys

equal :: (Eq a) => Set a -> Set a -> Bool
equal (Set xs) (Set ys) = xs == ys

--task 9
-- Да се дефинира алгебричен тип данни, представящ речник от ключове и стойности, където 
--всеки ключ е уникален. Да се дефинират следните функции:
-- fromList :: [(k,v)] -> Dict k v;
-- toList :: Dict k v -> [(k,v)];
-- insert :: k -> v -> Dict k v -> Dict k v;
-- delete :: k -> Dict k v -> Dict k v;
-- lookup :: k -> Dict k v -> Maybe v;
-- merge :: Dict k v -> Dict k v -> Dict k v, която слива два речника така, че ако на един и същи ключ съответстват различни 
--стойности, се избира тази от втория речник.
-- Забележка: Някои от типовете на горните функции са непълни. Допълнете ги по такъв начин, че да се компилират и работят коректно.

newtype Dict k v = Dict [(k, v)]
  deriving (Show)


