module Sol6 where

--task 1
--Да се дефинира функция, която приема списък и връща 
--списък от всички подмножества на списъка.

subsets :: [a]-> [[a]]
subsets [] = [[]]
subsets (x:xs) = map (x:) (subsets xs) ++ subsets xs

--task 2
--Да се дефинира безкраен поток 
--от всички наредени двойки естествени числа.

natPairs :: [(Int, Int)]
natPairs = [ (x, n-x) | n<-[0..], x<-[0..n] ]

--task 3
--Да се дефинира безкраен поток от всички наредени двойки цели числа.

intPairs :: [(Int, Int)]
intPairs = [ (x, y) | n<-[0..], x<-[-n..n], y<-[-n..n], abs x + abs y == n ]

--task 4
--Да се дефинира безкраен списък от всички степени на двойката.

powerOfTwo ::[Int]
powerOfTwo = map (2^) [0..]

powerOfTwo' ::[Int]
powerOfTwo' = 1: map (*2) powerOfTwo'

--task 5
--Да се дефинира безкраен поток от числата на Фибоначи.

fibs::[Int]
fibs = 0:1: zipWith (+) fibs (tail fibs)

--task 6
--Да се дефинира безкраен списък от факториелите на всички естествени числа.

factoriel :: Int -> Int
factoriel n = product [1..n]

facts :: [Int]
facts = [factoriel n | n<-[0..]]

facts' :: [Int]
facts' = map factoriel [0..]

facts'' :: [Int]
facts'' = 1: zipWith (*) [2..]  facts''

--task 7
--Да се дефинира безкраен списък от всички триъгълни числа.

triangleNum ::[Int]
triangleNum = scanl (+) 0 [1..]

--task 8
--Да се дефинира безкраен поток от всички Питагорови тройки.

pitagorTriples ::[(Int,Int,Int)]
pitagorTriples = [(a,b,c) |  c <-[5 ..], a<- [1..c], b<-[a..c], a^2 +b^2 == c^2, gcd a b ==1]

--task 9
--Да се дефинира безкраен поток от всички прости числа.
--За колко начина на имплементация се сещате?

isPrime :: Int ->Bool
isPrime n = length (filter (\x -> n `mod` x == 0 ) [2..n-1] )== 0

isPrime' :: Int ->Bool
isPrime' n = all ((/=) 0.( n `mod`)) [2..n-1]

primes :: [Int]
primes = filter isPrime [2..]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) =  x :sieve(filter(\y -> y `mod` x /=0) xs)

primes' :: [Int]
primes' = sieve [2..]

