module Sbornik where
import Prelude hiding (foldl, map, foldr, filter)
import Data.List (nub,)
import Data.Char (toUpper)

--22.1 
trianglePerimeter :: Double -> Double -> Double -> Double
trianglePerimeter x y z = x + y + z

triangleArea :: Double -> Double -> Double -> Double
triangleArea x y z = sqrt ( halfper * (halfper - x) * (halfper - y) * (halfper - z) )
    where
        per = trianglePerimeter x y z
        halfper = per/2

type Point = (Double, Double)
sideLen :: Point -> Point -> Double
sideLen x y = sqrt ((fst x- fst y)^2 + (snd x - snd y)^2)

triangleArea' :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
triangleArea' x y z = triangleArea a b c
    where
        a = sideLen x y
        b = sideLen y z
        c = sideLen z x
--22.2 
coordinate:: Point -> Int
coordinate (x,y)
    | x<0 = if y<0 then 3 else 4
    | x>0 = if y<0 then 2 else 1
    | x == 0 = 5
--22.3
del :: Int -> Bool
del p = p `mod` 4 ==0 || p `mod` 7 ==0

noCoren:: Double -> Double -> Double -> Bool
noCoren a b c = (b*b - 4*a*c) <0

isInCircle01R5 :: Double -> Double -> Bool
isInCircle01R5 a b = (a*a + (b - 1)^2) < 25

isOutsideCircle :: Double -> Double -> Double -> Double -> Double -> Bool
isOutsideCircle a b c d f = ((a - c)^2 + (b - d)^2) > (f*f)

isInThirdQuadrantR5 :: Double -> Double -> Bool
isInThirdQuadrantR5 a b =
    (a^2 + b^2 <= 25) && (a <= 0) && (b <= 0)

isInAnnulus :: Double -> Double -> Bool
isInAnnulus a b =
    let distSq = a*a + b*b
    in (distSq >= 25) && (distSq <= 100)

isIn :: Int -> Bool
isIn x = x>=0 && x<=1

isMax :: Int -> Int -> Int -> Int -> Bool
isMax x a b c = x == max a (max b c)

isNotMax :: Int -> Int -> Int -> Int -> Bool
isNotMax x a b c = x /= max a (max b c)

nonePositive :: (Num a, Ord a) => a -> a -> a -> Bool
nonePositive a b c = (a <= 0) && (b <= 0) && (c <= 0)

hasDigit7 :: Int -> Bool
hasDigit7 p = '7' `elem` show p

digitsAreDistinct :: Int -> Bool
digitsAreDistinct m =
    let digits = show m
    in length digits == length (nub digits)

--22.4
digitsCount :: Int -> Int
digitsCount 0 = 0
digitsCount n = 1 + digitsCount (div n 10)
-- digitsCount x = length(show x)

--22.5
digitSum :: Int -> Int
digitSum 0 = 0
digitSum n = digitSum (div n 10) + mod n 10

--22.6
pow :: Int -> Int -> Int
pow _ 0 = 1
pow x k = x * pow x (k -1)

--22.7
countKinN :: Int -> Int->Int
countKinN _ 0 = 0
countKinN k n
    | k == lastDigit = 1+ countKinN k (n `div` 10)
    | otherwise = countKinN k (n `div` 10)
    where
        lastDigit = n `mod` 10
--22.8
isLeapYear :: Int -> Bool
isLeapYear year =
    (year `mod` 400 == 0) || (year `mod` 4 == 0 && year `mod` 100 /= 0)

--22.9 

--22.10
isPowerOf :: Int -> Int -> Bool
isPowerOf n k
    | n == 1       = True
    | k <= 1       = False
    | n `mod` k /= 0 = False
    | otherwise    = isPowerOf (n `div` k) k

--22.11


--23.1
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' n []= False
elem' n (x:xs) = n == x || elem' n xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

--23.2
count' :: (Eq a) => a -> [a] -> Int
count' _ [] = 0
count' el (x:xs)
    | el == x = 1+ count' el xs
    | otherwise = count' el xs

--23.3
index :: Eq a => a -> [a] -> Int
index el lst = helper el lst 1
    where
        helper :: (Eq a ) => a -> [a] -> Int -> Int
        helper target (x:xs) acc
            | target == x = acc
            | otherwise = helper target xs acc+1

--23.4
sublist:: Eq a => [a] -> [a] ->Bool
sublist [] _ = True
sublist _ [] = False
sublist (x:xs) l2 = x `elem` l2 && sublist xs l2

--23.5
common:: Eq a => [a] -> [a] ->Int
common [] _ = 0
common _ [] = 0
common (x:xs) l2
    |x `elem` l2 = 1 + common xs l2
    | otherwise = common xs l2

--23.6
duplicates :: Eq a => [a] -> Bool
duplicates (x:xs) = x `elem` xs && duplicates xs

--24.1
evens :: Int ->  [Int]
evens  n = [2,4..( n*2)]

aritm :: Int -> Int -> Int -> [Int]
aritm a d n = take n [a, a+d..]

factoriel:: Int -> Int
factoriel 0 = 1
factoriel n = n * factoriel (n-1)

facts :: Int -> [Int]
facts n = map factoriel [1..n]

evens' :: [Int]
evens' = [2,4..]

aritm' :: Int -> Int -> [Int]
aritm' a d = [a,a+d..]

facts' :: [Int]
facts' = map factoriel [1..]

--24.2
digits :: Int -> [Int]
digits 0 =[]
digits n = n `mod` 10 : digits (n `div` 10)

--24.3
uniqueDigitsRightToLeft :: Int -> [Int]
uniqueDigitsRightToLeft n = nub (digits n)

--24.4

--24.5

--25.1
say :: Int -> String
say 0 = "zero"
say 1 = "one"
say 2 = "two"
say 3 = "three"
say 4 = "four"
say 5 = "five"
say 6 = "six"
say 7 = "seven"
say 8 = "eight"
say 9 = "nine"
say _ = "not a single digit"

--25.2
commonPrefixLength :: Eq a => [a] -> [a] -> Int
commonPrefixLength [] _ = 0
commonPrefixLength _ [] = 0
commonPrefixLength (x:xs) (y:ys)
    | x == y = 1 + commonPrefixLength xs ys
    | otherwise =0
--25.3
countEvenOdd :: [Int] -> (Int,Int)
countEvenOdd lst = helper (0,0) lst
    where
        helper (even, odd) [] = (even,odd)
        helper (even,odd) (x:xs)
            | x `mod` 2 == 0 = helper (even+1, odd) xs
            | otherwise = helper (even, odd + 1) xs

--25.4
pivot :: Ord p => p -> [p] -> ([p], [p])
pivot x lst =(l1,l2)
    where
        l1 = filter (<x) lst
        l2 = filter (>=x) lst

--25.5 
toUpperCase :: String -> String
toUpperCase = map toUpper

--25.6
palindrome :: Eq a => [a] -> Bool
palindrome x = x == reverse x

--25.7
adjacentPairsLess :: Ord b => [b] -> [(b, b)]
adjacentPairsLess l = [(a,b)| (a,b) <- zip l (tail l ), a<b]

--25.8
--26.1
data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Show, Eq, Ord, Enum, Bounded)

nextDay:: DayOfWeek ->DayOfWeek
nextDay Sun = Mon
nextDay d = succ d

previousDay :: DayOfWeek -> DayOfWeek
previousDay Mon = Sun
previousDay day    = pred day

dayToInt :: DayOfWeek -> Int
dayToInt d = fromEnum d + 1

intToDay :: Int -> Maybe DayOfWeek
intToDay n
    | n >= 1 && n <= 7 = Just (toEnum (n - 1))
    | otherwise        = Nothing

dayInterval :: DayOfWeek -> DayOfWeek -> [DayOfWeek]
dayInterval start end
    | start == end = [start]
    | start < end  = [start .. end]
    | otherwise    = [start .. Sun] ++ [Mon .. end]

--26.2
data Member = Student
    { studentFn    :: Int
    , studentName  :: String
    , studentCourses :: [String]
    }
    | Professor
    { professorName  :: String
    , professorCourses :: [String]
    , professorOffice  :: Int
    }
    deriving (Show)

countOfStudent:: [Member] -> Int
countOfStudent m = length $ filter isStudent m
    where
        isStudent :: Member -> Bool
        isStudent (Student {} ) = True
        isStudent _ = False



--27.1
type Config = (Int, Int)

--27.2
isValid :: Config -> Bool
isValid (left, right) =  left<100 && right <100 && abs (right-left) <50

addLeft::Int->Config ->Maybe Config
addLeft weightToAdd (left,right) =
    if isValid newConfig then Just newConfig else Nothing
    where newConfig = (left+weightToAdd, right)

addRight::Int->Config ->Maybe Config
addRight weightToAdd (left,right) =
    if isValid newConfig then Just newConfig else Nothing
    where newConfig = (left, right+weightToAdd)

--27.3
successfulChain :: Maybe Config
successfulChain =
    Just (5, 3) >>= (addLeft 10) >>= (addRight 10) >>= (addRight 3)

--27.4
state :: Config ->(Config -> Int)
state (left,right) = if left>=right then fst else snd

--28.1
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
foldr f z (x:xs) = f x (foldr f z xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

--28.2
sum'' :: [(Int,Int,Int)] -> [Int]
sum'' lst = map (\(x, y, z) ->  x +y + z ) lst

sumComponents :: [(Int, Int, Int)] -> (Int, Int, Int)
sumComponents = foldl sumT (0,0,0)

    where
        sumT ::(Int,Int,Int) -> (Int,Int,Int)-> (Int,Int,Int)
        sumT (sumA,sumB,sumC) (a,b,c) = (sumA+a, sumB+b, sumC+c )

countTriples :: [(Int,Int,Int)] -> Int
countTriples lst = length ( ( filter (\(a,b,c) -> a+b >c) lst ))

isEqualTriple :: [(Int,Int,Int)] ->Bool
isEqualTriple lst = (length $ filter (\(a,b,c) -> a==b && b==c ) lst) >0
-- >>> isEqualTriple [(1,1,0)]
-- False

--28.3
validIndex :: [Int] -> [Int]
validIndex lst = map fst $ filter (\(num, ind) -> num == ind) indexList
    where
        indexList = zip lst [1..]

--28.4
sumSequence :: [Int] ->[Int]
sumSequence lst@(x:xs) = map (\(x,y) -> x+y) (zip lst xs)

--28.5
sums :: [Int] -> [Int]
sums list = snd $ foldl (\(acc,results) x ->
    let newSum = acc + x
    in (newSum, results ++ [newSum])) (0, []) list

--28.6
separate :: (a -> Bool) ->[a] ->([a],[a])
separate p (x:xs)
    | p x = (x:pref,suf)
    | otherwise = ([], x:xs)
        where
            (pref,suf) = separate p xs
--separate p lst = span p lst

--28.7
mySplit :: (a -> Bool) -> [a] -> [[a]]
mySplit p [] = []
mySplit p list =
    let (prefix, rest) = separate p list
    in case rest of
        []     -> [prefix]
        (x:xs) -> prefix : mySplit p xs

--28.8
type Pos = ( Int , Int )
data Tile = Road | Wall | Gold deriving (Show, Eq)
data Game = Game { pos :: Pos , world :: [[Tile]] } deriving (Show)

countWalls :: Game ->Int
countWalls (Game _ world) = sum $ map (length.filter (==Wall)) world

