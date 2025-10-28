module Week2gr1 where

--task1
sum' 0 =0
sum' n = n + sum' (n-1)

--task 2
countDigits 0 =0
countDigits n = 1 + countDigits (div n 10)

--task 3
divisorSum n = helper n 2 
    where
        helper 0 _ =0
        helper n curr
        | curr>=n =0 
        | div n curr ==0 =curr + helper (div ncurr) curr 
        | div n curr /=0 = helper n (curr+1)