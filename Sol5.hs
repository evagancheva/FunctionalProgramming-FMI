module Sol5 where
import Prelude hiding (scanr, iterate, zipWith, filter, flip, map, foldr,foldl,all, any, concatMap,curry, uncurry, nub)
--task 0
--Да се дефинира функция compose, която приема две едноместни функции и връща 
--нова функция, която е тяхната композиция. Какъв трябва да е типът на compose?

compose :: (a -> b) -> (b -> c) -> (a -> c)
compose f g  = (\x -> g (f x))

--task 1
--Да се дефинират следните функции от по-висок ред от стандартната библиотека (Prelude):

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs)
    | f x = x :filter f xs
    | otherwise = filter f xs


foldr :: (a -> b -> b) -> b -> [a] -> b
foldr  _ z [] = z
foldr op z (x:xs) = op x (foldr op z xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p lst = foldr (\x xs -> if p x then x : xs else xs) [] lst

map' :: (a -> b) -> [a] -> [b]
map' f lst = foldr (\x xs -> f x : xs) [] lst

all :: (a -> Bool) -> [a] -> Bool
all p lst= foldr (\x xs -> p x && xs) True lst

any :: (a -> Bool) -> [a] -> Bool
any p lst= foldr (\x xs -> p x || xs) False lst

concatMap :: (a -> [b]) -> [a] -> [b]
-- concatMap f (x:xs) = f x ++ concatMap f xs
concatMap f lst = foldr (\x xs -> f x ++ xs) [] lst

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f l1 l2 = foldr ((:) . uncurry f) [] $ l1 `zip` l2

curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x,y)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x,y) = f x y

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

unfoldr :: (b -> Maybe (a,b)) -> b -> [a]
unfoldr f b =
    case f  b of
        Nothing ->[]
        Just(a, c) -> a : unfoldr f c

--task 2 
(&) :: a -> (a->b) -> b
(&) x f = f x

--task 3
(&&&) :: (a ->b) -> ( a -> c) -> a ->(b,c)
(&&&) f g  = \x -> (f x , g x)

--task 4
fixpoint:: Eq a => (a -> a) -> [a] ->[a]
fixpoint f lst = foldr (\x -> if f x == x then (x:) else id) [] lst

-- fixpoint _ [] = []
-- fixpoint f (x:xs)
--     | f x == x = x : fixpoint f xs
--     | otherwise = fixpoint f xs

--task 5
scanr :: (a->b->b)->b ->[a] ->[b]
scanr f z lst = foldr (\x acc -> f x (head acc) : acc) [z] lst

--task 6
composeList :: [a->a] -> (a ->a)
composeList = foldr (.) id

-- composeList [] =id
-- composeList (x:xs) = x . composeList xs

--task 7
-- Да се дефинира функция, която имплементира алгоритъма за сортиране чрез 
-- пряка селекция, използвайки само функции от по-висок ред.

selectionSort :: (Ord a ) => [a]->[a]
selectionSort [] = []
selectionSort lst = l1 ++ selectionSort l2
    where
        minEl = minimum lst
        l1 = filter (== minEl) lst
        l2 = filter (/= minEl) lst


--task 8
-- Да се дефинира функция, която имплементира алгоритъма за сортиране 
-- чрез вмъкване, използвайки само функции от по-висок ред.
insertOrdered :: (Ord a) => a -> [a] -> [a]
insertOrdered x [] = [x]
insertOrdered x (y:ys)
    | y<=x = y: insertOrdered x ys
    | otherwise = x:y:ys

insertionSort:: (Ord a) => [a] ->[a]
insertionSort = helper []
    where
        helper:: (Ord a) => [a]->[a] ->[a]
        helper sorted (x:xs) = helper (insertOrdered x sorted) xs
--task 9
-- Да се дефинира функция, която имплементира алгоритъма за бързо сортиране.
-- Бонус: Функцията да приема двуместен предикат, който да дефинира наредбата
--  между елементите в списъка.

quickSortBy:: (a->a->Bool) -> [a] ->[a]
quickSortBy _ [] = []
quickSortBy cmp (x:xs) = quickSortBy cmp leq ++ [x] ++ quickSortBy cmp gt
    where
        leq = filter (cmp x) xs
        gt = filter (\y -> not ( cmp y x)) xs
--task 10
-- Да се дефинира функция, която приема едноместен предикат и списък от елементи 
-- от произволен тип и връща индекса и стойността на първия елемент в списъка, 
-- който удовлетворява дадения предикат.

findFirst :: (a -> Bool) ->[a] ->Maybe(Int,a)
findFirst p lst = foldr (\el@(_,x) xs -> if p x then Just el else xs) Nothing (zip [0..] lst)

--task 11
-- Да се дефинира функция, която приема естествено положително число n и списък 
-- от числа, задаващ интервал, и връща списък от всички подинтервали на оригиналния
--  с дължина n.

suffixes :: [a] -> [[a]]
suffixes lst = scanr (:) [] lst

intervalsN :: Int -> [Int] ->[[Int]]
intervalsN n lst = map (take n) $ filter((>=n) . length) (suffixes lst) 

--task 12
-- Да се дефинира безкраен поток от всички цели числа, като всяко цяло число 
-- трябва да има свой индекс в потока.

integers:: [Int]
integers = 0: concatMap  (\x-> [x,-x] ) [1..]

-- >>> take 10 integers

-- ProgressCancelledException
-- [0,1,-1,2,-2,3,-3,4,-4,5]
