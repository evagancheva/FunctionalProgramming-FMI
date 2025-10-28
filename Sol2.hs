module Sol2 where
import Prelude hiding(reverse, take,drop)
--task 1

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
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

--task 4
append :: [a] -> [a] -> [a]
append [] ys =ys
append (x:xs) ys = x : append xs ys 

--task 5
take :: Int ->[Int] -> [Int]
take 0 _ =[]
take _ [] =[]
take a (x:xs) = x : take (pred a) xs

drop ::  Int ->[Int] -> [Int]
drop 0 xs = xs
drop a (x:xs) = drop (a-1) xs

--task 6
split :: (Eq a) => a -> [a] -> [[a]]
split delim lst = helper [] delim lst
  where
    helper :: (Eq a) => [a] -> a -> [a] -> [[a]]
    helper acc _ [] = [acc]
    helper acc delim (x : xs)
      | x == delim = if null acc then helper [] delim xs else acc : helper [] delim xs
      | otherwise = helper (acc ++ [x]) delim xs

--task 7 
splitEvenOdd :: [a] -> ([a],[a])
splitEvenOdd (x:xs) = helper [] [] (x:xs)
    where
        helper even odd [] = (even,odd)
        helper evenAcc oddAcc [x] = (evenAcc ++ [x], oddAcc)
        helper even odd (x:y:xs) = helper (even ++ [x]) (odd++[y]) xs

-- >>> splitEvenOdd [1,5,8,10,6]
-- ([1,8,6],[5,10])

--task 8
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
removeConsecutive :: (Eq a) => [a] -> [a]
removeConsecutive [] = []
removeConsecutive [x] = [x]
removeConsecutive (x:y:xs) = if x == y 
    then removeConsecutive (y:xs)
    else x : removeConsecutive(y:xs)
-- >>> removeConsecutive [1,1,2,2,2,2,4,1,1]
-- [1,2,4,1]

--task10
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
