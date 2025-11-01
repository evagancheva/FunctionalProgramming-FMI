module Sol2 where
import Prelude hiding(reverse, take,drop)
--task 1
--Да се дефинира функция, която приема цяло число и проверява дали то е палиндром.

reverse :: Int ->Int
reverse x = helper x 0
    where 
        helper :: Int->Int->Int
        helper 0 res = res
        helper x res
            | x<10 = res*10 +x 
            | otherwise  = helper (quot x 10) (res*10 + rem x 10)


palindrome :: Int -> Bool
palindrome x = x == reverse x 

--task 2
--Да се дефинира функция, която приема две естествени числа x и n и връща по колко 
-- различни начина числото x може да се представи като сума на различни числа, всяко 
-- повдигнато на степен n.

sumOfNthPowers :: Int -> Int ->Int
sumOfNthPowers x n = helper 0 x
    where 
        helper::Int->Int->Int
        helper curr rest 
            | curr^n> rest =0
            | rest == curr^n =1
            | otherwise = helper (curr+1) rest + helper (curr+1) (rest - curr^n)

-- >>> sumOfNthPowers (-1) 10
-- 0
--task 3
--Да се дефинира функция, която намира сумата на елементите в списък от цели числа.

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

--task 4
-- Да се дефинира функция, която приема два списъка като аргументи и залепя двата 
--списъка един за друг. Да не се използва операторът ++.

append :: [a] -> [a] -> [a]
append [] ys =ys
append (x:xs) ys = x : append xs ys 

--task 5
--Да се дефинират функциите take и drop от стандартната библиотека:

take :: Int ->[Int] -> [Int]
take 0 _ =[]
take _ [] =[]
take a (x:xs) = x : take (pred a) xs

drop ::  Int ->[Int] -> [Int]
drop 0 xs = xs
drop a (x:xs) = drop (a-1) xs

--task 6
-- Да се дефинира функция, която приема елемент, който ще наричаме разделител, от
-- някакъв тип и списък с елементи от този тип и връща списък от списъци, получен при 
-- "разделянето" на оригиналния списък спрямо разделитетя.

split :: (Eq a) => a -> [a] -> [[a]]
split delim lst = helper [] delim lst
  where
    helper :: (Eq a) => [a] -> a -> [a] -> [[a]]
    helper acc _ [] = [acc]
    helper acc delim (x : xs)
      | x == delim = if null acc then helper [] delim xs else acc : helper [] delim xs
      | otherwise = helper (acc ++ [x]) delim xs

--task 7 
-- Да се дефинира функция, която приема списък и връща два списъка, първият от които 
-- съдържа елементите на четните индекси, а вторият - елементите на нечетните индекси 
-- в оригиналния списък.

splitEvenOdd :: [a] -> ([a],[a])
splitEvenOdd (x:xs) = helper [] [] (x:xs)
    where
        helper even odd [] = (even,odd)
        helper evenAcc oddAcc [x] = (evenAcc ++ [x], oddAcc)
        helper even odd (x:y:xs) = helper (even ++ [x]) (odd++[y]) xs

-- >>> splitEvenOdd [1,5,8,10,6]
-- ([1,8,6],[5,10])

--task 8
--Да се дефинират следните две функции, които приемат списък като единствен аргумент:
--suffixes - връща списък от суфиксите на списъка;
--prefixes - връща списък от префиксите на списъка.

suffixes :: [a] -> [[a]]
suffixes [] =[[]]
suffixes l@(x:xs) =  l:suffixes xs

-- >>> suffixes [1,2,3]
-- [[1,2,3],[2,3],[3],[]]

prefixes :: [a] ->[[a]]
prefixes  = helper [] 
    where
        helper :: [a] -> [a] -> [[a]]
        helper acc [] = [acc]
        helper acc (x:xs) = acc : helper (acc ++ [x]) xs

-- >>> prefixes [1,2,3]
-- [[],[1],[1,2],[1,2,3]]

--task9
--Да се дефинира функция, която премахва всички последователни срещания на елементите в списък.

removeConsecutive :: (Eq a) => [a] -> [a]
removeConsecutive [] = []
removeConsecutive [x] = [x]
removeConsecutive (x:y:xs) = if x == y 
    then removeConsecutive (y:xs)
    else x : removeConsecutive(y:xs)
-- >>> removeConsecutive [1,1,2,2,2,2,4,1,1]
-- [1,2,4,1]

--task10
-- Да се напише функция, която приема списък от елементи и връща списък от списъци,
--  където всеки списък се състои от последователните срещания на един и същ елемент в 
--  списъка.

pack :: Eq a => [a] -> [[a]]
pack (x:y:xs) = helper [] (x:y:xs)
    where
        helper :: (Eq a) => [a] -> [a] -> [[a]]
        helper acc [] = [acc]
        helper acc [x] = [acc ++ [x]]
        helper acc (x:y:xs) 
            | x == y = helper ( acc ++[x])  (y:xs)
            | otherwise =(acc ++[x]) : helper [] (y:xs)

-- >>> pack "aabbbcdda"
-- ["aa","bbb","c","dd","a"]

--task 11
-- Да се дефинира функция, която намира n-тия член на редица, образуваща се по
--  следния начин:
-- Нека 
-- ai е член на редицата. Тогава:ai=
-- {0 , i=0
-- {ai−1 − i , ai−1 − i > 0 ∧ ai−1 − i ∉a0,...,ai−1
-- {ai−1 + i ,иначе
