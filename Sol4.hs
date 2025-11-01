module Sol4 where
import Prelude hiding (zip)

--task 1
--Да се дефинира функцията zip, която приема два списъка и връща списък от 
--наредени двойки от елементите им.
zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys
-- >>> zip [1,2,3] "abcd"
-- [(1,'a'),(2,'b'),(3,'c')]

--task 2
--Да се дефинира функция, която приема списък от списъци от произволен тип и връща 
-- най-дългия списък. Ако има няколко списъка с еднаква дължина, да се върне първият 
-- такъв.

longest :: [[a]] -> [a]
longest [] = []
longest (x:xs) = helper x xs
    where
        helper:: [a]->[[a]] ->[a]
        helper current [] = current
        helper current (y:ys)
            | length y > length current = helper y ys
            | otherwise =helper current ys

-- >>> longest [[1,2,3],[4,5],[6,7,8,9],[10]]
-- [6,7,8,9]

--task 3
--Да се дефинира функция, която имплементира алгоритъма за сортиране чрез пряка селекция.

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs =
    let m = minimum xs in m: selectionSort ( removeMin m xs)
        where
            removeMin::Ord a => a -> [a] -> [a]
            removeMin _ [] = []
            removeMin mi (y:ys)
                | mi == y = ys
                | otherwise = y : removeMin mi ys

-- >>> selectionSort [3,1,4,1,5,9,2,6]
-- [1,1,2,3,4,5,6,9]

--task 4
-- Да се дефинира функция, която приема два списъка с елементи от произволни 
-- типове и връща списък, представящ тяхното декартово произведение.

dekart:: [a] -> [b] -> [(a,b)]
dekart [] _ = []
dekart _ [] = []
dekart (x:xs) ys = helper x ys ++ dekart xs ys
    where
        helper:: a -> [b] -> [(a,b)]
        helper a [] = []
        helper a (b:bs) = (a,b) : helper a bs

-- >>> dekart [1,2,3] "ab"
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]

--task 5
-- Да се дефинира функция, която приема списък с елементи от произволен тип 
-- и връща хистограма на списъка. Хистограма ще наричаме списък от наредени 
-- двойки, където първата компонента е колко пъти се среща в списъка даден 
-- елемент, а втората - кой е елементът.

histogram :: Eq a => [a] ->[(Int,a)]
histogram [] = []
histogram (x:xs) =
    let count = 1 + countElem x xs
        rest = removeAll x xs
    in (count,x) : histogram rest
    where
        countElem::Eq a => a -> [a] -> Int
        countElem _ [] = 0
        countElem y (z:zs)
            | y == z = 1 + countElem y zs
            | otherwise = countElem y zs

        removeAll :: Eq a =>a->[a] ->[a]
        removeAll y [] = []
        removeAll y (z:zs)
            | y == z = removeAll y zs
            | otherwise =z : removeAll y zs

-- >>> histogram "abcddaba"
-- [(3,'a'),(2,'b'),(1,'c'),(2,'d')]

--task 6
--Да се дефинира функция, генерираща безкраен списък от двоичните 
--записи на положителните естествени числа.

binary :: [String]
binary = helper ["1"]
    where 
        helper (x:xs) = x: helper ( xs ++ [x++"0", x++"1"])

-- >>>take 10 binary
-- ["1","10","11","100","101","110","111","1000","1001","1010"]

--task 7
-- Да се дефинира алгебричен тип данни, представящ матрица, чиито елементи са числа,
-- с произволни размерности. Да се дефинират следните функции:
-- isMatrix :: Matrix -> Bool, която проверява дали матрицата е валидна;
-- addMatrices :: Matrix -> Matrix -> Matrix;
-- multMatrices :: Matrix -> Matrix -> Matrix;
-- transpose :: Matrix -> Matrix.

type Matrix a = [[a]]

isMatrix :: Matrix a -> Bool
isMatrix []= False
isMatrix (m:ms)= helper (length m) ms
    where
        helper:: Int ->[[a]] ->Bool
        helper _ [] = True
        helper n (x:xs) = 
            length x == n && helper n xs

-- >>>isMatrix [[1,2,3],[4,5,6]] 
-- True


