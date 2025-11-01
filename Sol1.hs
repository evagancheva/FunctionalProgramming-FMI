module Sol1 where
import Prelude hiding (succ, pred, add,exp)


--task 1 
--Да се дефинират функции, които пресмятат периметъра и лицето на триъгълник в 
--равнината по подадени негови дължини на три страни.

trianglePerimeter :: Double -> Double -> Double -> Double
trianglePerimeter x y z = x + y + z

-- >>> trianglePerimeter 2 2 3
-- 7.0

triangleArea :: Double -> Double -> Double -> Double
triangleArea x y z = sqrt ( halfper * (halfper - x) * (halfper - y) * (halfper - z) )
    where
        per = trianglePerimeter x y z
        halfper = per/2

-- >>> triangleArea 2 2 3
-- 1.984313483298443

--task 2
-- Да се дефинират същите функции от Задача 01, които работят върху три точки в 
-- равнината вместо дължини на страни.

type Point = (Double, Double)
sideLen :: Point -> Point -> Double
sideLen x y = sqrt ((fst x- fst y)^2 + (snd x - snd y)^2)

trianglePerimeter' :: Point -> Point -> Point -> Double
trianglePerimeter' x y z = trianglePerimeter a b c 
    where   
        a = sideLen x y 
        b = sideLen y z 
        c = sideLen z x
-- >>> trianglePerimeter' (0,1) (2,8) (5,6)
-- 17.95672897660998

triangleArea' :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
triangleArea' x y z = triangleArea a b c 
    where
        a = sideLen x y 
        b = sideLen y z 
        c = sideLen z x 
-- >>> triangleArea' (0,1) (2,8) (5,6)
-- 12.49999999999999


--task3
--Да се дефинира функция, която приема като единствен параметър данните на студент 
--от ФМИ - име, факултетен номер, специалност и курс, и връща форматиран низ с 
--информацията на студента.

printStudent :: (String, String, String, Int) -> String
printStudent (name, fn, major, year) = 
    "This is "++ name 
    ++ " with a faculty number of " ++ fn
    ++ " who is in year " ++ show year ++ " of " ++ major


-- >>> printStudent ("imbadwithnames", "99999", "Computer Science", 3)
-- "This is imbadwithnames with a faculty number of 99999 who is in year 3 of Computer Science"

--task 4
-- --Да се дефинира функцията succ, която приема естествено число и връща естественото 
-- число, получено чрез прибавяне на единица. Без да използвате други функции освен 
-- указаната, дефинирайте следните функции:
--pred - за естествено число n връща n-1. Ако n==0, връща 0;
--add - прибавя две естествени числа;
--mult - умножава две естествени числа;
--exp - повдига първия си аргумент на степен втория си аргумент.

succ :: Int  -> Int
succ  x = x + 1

pred :: Int -> Int
pred n 
    |n ==0 =0
    |otherwise = helper 0 n 
    where 
        helper :: Int -> Int -> Int
        helper m x 
            | succ m == x =m
            | otherwise = helper (succ m) x

-- >>> pred 6
-- 5

add :: Int -> Int -> Int
add x y 
    | y == 0 =x 
    | otherwise  = succ(add x (pred y))

-- >>> add 99 21
-- 120

mult :: Int -> Int -> Int
mult x y 
    | y == 0 =0
    | otherwise =add x (mult x (pred y))

-- >>> mult 10 12
-- 120

exp :: Int -> Int -> Int
exp x y 
    | y==0 =1
    | otherwise = mult x (exp x (pred y))
    
-- >>> exp 2 12
-- ProgressCancelledException

--task 5
--Да се дефинира функция, която намира сбора от цифрите на произволно цяло число.

digitSum :: Int -> Int
digitSum 0 = 0
digitSum n = digitSum (div n 10) + mod n 10

-- >>> digitSum 12345
-- 15

--task 6
-- Да се дефинира функция, която намира сбора на естествените числа в интервала, 
-- определен от двата параметъра.

intervalSum :: Int -> Int -> Int
intervalSum x y
    | x == y =y
    | otherwise = add x (intervalSum (succ x) y)

-- >>> intervalSum (-1) 16
-- 135

--task 7
-- Да се дефинират функции, които намират факториел и двоен факториел от подадено 
-- като параметър естествено число.
fact :: Int -> Int
fact 0 = 1
fact 1 = 1
fact n = n * fact(n-1)

-- >>> fact 6
-- 720

doubleFact 0 = 1
doubleFact 1 = 1
doubleFact n = n * doubleFact (n-2)

-- ProgressCancelledException
-- 48

--task 8
-- Да се дефинират функции, които намират най-големия общ делител и най-малкото 
-- общо кратно на две естествени числа.

gcd' :: Int -> Int ->Int
gcd' a b 
    | a==0 =b
    | b==0 =a
    | a > b = gcd' b (a `mod` b)
    | otherwise = gcd' a (b `mod` a)
-- >>> gcd' 16 8
-- 8
-- >>> gcd' 15 9
-- 3
lcm' :: Int -> Int -> Int
lcm' x y = div (x*y) (gcd' x y )
-- >>> lcm' 12 16
-- 48

--task 9 
-- Да се дефинира функция, която проверява дали две цели числа са взаимно прости. 
-- Чрез нея да се дефинира функцията на Ойлер, която приема естествено число n и 
-- пресмята броя взаимно прости с n числа в интервала [1,n−1].

coprime :: Int -> Int -> Bool
coprime x y 
    | gcd' x y == 1 =True
    | otherwise =False
-- >>> coprime 4 5
-- True

totient :: Int -> Int
totient  = helpe 1 
    where
        helpe :: Int ->Int ->Int
        helpe a b  
            | a >= b =0
            | coprime a b = 1 + helpe (succ a) b
            | otherwise =helpe (succ a) b

-- >>> totient 7
-- 6

--task10
-- Да се напише функция, която приема положително четно число, по-голямо от 2, и 
-- връща двойка прости числа, чиято сума е равна на даденото число.

prime :: Int -> Bool
prime n 
    | n< 2 =False
    | n==2 =True
    | otherwise = helpi 2 
        where 
            helpi :: Int->Bool
            helpi a 
                | a> floor(sqrt(fromIntegral n)) = True
                | mod n a == 0 =False
                | otherwise =helpi (a+1)  

                -- otherwise =mod n a /= 0 && helpi(a+1)

goldbach :: Int ->(Int,Int)
goldbach n 
    | n<=2 =(0,0)
    | otherwise = helper 3 n
        where
            helper :: Int -> Int ->(Int, Int)
            helper a b 
                | prime a =(a, b-a)
                | otherwise =helper (a+1) b
-- >>> goldbach 14
-- (3,11)
