fact::Int -> Int
fact 0  = 1
fact n = n*fact (n-1)

fib:: Int -> Int
fib 0 =1
fib 1 =1
fib n = fib (n-1) + fib (n-2)

myAbs:: Int->Int
myAbs n
    | n < 0 = -n
    | otherwise = n

composeInt :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
composeInt f g = result
    where
        result :: Int -> Int
        result x = f (g x)

compose :: (c->b) -> ( a -> c) -> (a->b)
compose f g = result
    where
        result x = f (g x)

myConcat ::[a] ->[a]->[a]
myConcat [] ys = ys
myConcat (x:xs) ys = x: myConcat xs ys

-- foldr (:) ys xs ==(1:(2:(3:ys)))

isIntPrefix :: [Int]->[Int]->Bool
isIntPrefix [] _ = True
isIntPrefix _ [] = False
isIntPrefix (x:xs) (y:ys) = x==y && isIntPrefix xs ys

isPrefix :: (Eq a) => [a]->[a]->Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = (x == y) && isPrefix xs ys

frepeat :: Int -> (a->a) ->a -> a
frepeat 0 _ x = x
frepeat n f x = f (frepeat (n-1) f x)

frepeated :: Int -> (a-> a) -> (a->a)
frepeated 1 f = f
frepeated n f = f .frepeated (n-1) f 

len :: [a] -> Int
len [] =0
len (x : xs) = 1 + len xs
-- len xs = foldr (\ x -> (+) 1) 0 xs

exists :: (a -> Bool) -> [a] -> Bool
exists _ [] = False
exists p (x:xs) = p x || exists p xs

forall :: (t -> Bool) -> [t] -> Bool
forall p [] = True
forall p (x:xs) = p x && forall p xs

member :: Eq t => t -> [t] -> Bool
member x [] = False
member x (y:ys) = x == y || member x ys 

listMap :: (t -> a) -> [t] -> [a]
listMap _ [] = []
listMap f (x:xs) = f x : listMap f xs

listFilter :: (a -> Bool) -> [a] -> [a]
listFilter _ [] = []
listFilter p (x:xs) 
    | p x = x: listFilter p xs
    | otherwise = listFilter p xs 

push :: t -> [t] -> [t]
push x [] = [x]
push y (x:xs) = x: push y xs 

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

insert :: (Eq t1, Num t1) => t2 -> t1 -> [t2] -> [t2]
insert x _ [] = [x]
insert x 0 lst = x:lst
insert y n (x:xs) = x : insert y (n-1) xs

append :: [a] -> [a] -> [a]
append [] l2 = l2
append (x:xs) l2 = x : append xs l2

