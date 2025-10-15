
module Week1gr1 where
import Distribution.Simple (anaVersionRange)

--task 1
sign :: Int  -> String
sign n
    |n ==0 ="zero"
    |n>0 ="positive"
    |otherwise ="negative"

-- >>>sign (-3)
-- "negative"

--task 2
roots :: Int -> Int->Int ->Int
roots a b c
    |d >0 =2
    |d==0 =1
    |otherwise =0
    where   
        d = b^2 -4*a*c

--task 3

superNumber :: Double-> Double -> Double -> Double
superNumber a b c = minNum*maxNum + averageNum
    where 
        minNum = minimum [a,b,c]
        maxNum = maximum [a,b,c]
        averageNum = a +b +c -minNum -maxNum

--task 4
modulus :: (Double,Double) -> Double 
modulus (a,b) = sqrt ( a^2 + b^2)

modComplexCurried :: Double -> Double -> Double
modComplexCurried = curry modulus

-- >>> modComplexCurried 3 4
-- 5.0

--task 5 

type Point =(Double,Double)
distance ::Point -> Point -> Double
distance a b = sqrt((fst a -fst b)^2 + (snd a -snd b)^2)

--task 6
compute :: (Int,Int ,Double) ->Double
compute (0, _, _) = 0
compute (a,b,c) = c/ fromIntegral a + fromIntegral b

--task 7 

(~=) :: Double ->Double ->Bool
(~=) a b = abs (b-a) < 0.000001

