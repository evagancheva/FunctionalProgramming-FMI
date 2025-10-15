module Sol2 where
import Prelude hiding(reverse)
--task 1

reverse :: Int ->Int
reverse x = helper x 0
    where 
        helper :: Int->Int->Int
        helper x res
            | x<10 = res*10 +x 
            | otherwise  = helper (div x 10) (res*10 + mod x 10)


palindrome :: Int -> Bool
palindrome x = x == reverse x 

--task 2
sumOfNthPowers :: Int -> Int ->Int
sumOfNthPowers = helper 0
    where 
        helper curr rest st
            | curr^st> rest =0
            | rest <0 =0
            | rest == 0 =1
            | otherwise = helper (curr+1) rest st + helper (curr+1) (rest - curr^st) st