module Hw1 where
import Data.Char ( digitToInt)

--task 1
data LDE2 = LDE2 Integer Integer Integer
              deriving (Show)

extendedEvklid :: Integer ->Integer ->(Integer,Integer,Integer)
extendedEvklid a 0 = (a,1,0)
extendedEvklid a b = (d',x,y)
    where
        (d',x',y') = extendedEvklid b (mod a b)
        x = y'
        y = x'- (div a b) * y'

concreteSolution :: LDE2 ->Maybe(Integer, Integer)
concreteSolution (LDE2 a b c )
    |mod c d /=0 = Nothing
    | otherwise = Just (x0,y0)
    where
        (d,x,y) = extendedEvklid a b
        k = div c d
        x0 = x*k
        y0 = y*k

checkSolution :: (Integer, Integer)->LDE2 ->Bool
checkSolution (x,y) (LDE2 a b c ) = a*x +b*y ==c

integer  :: [Integer]
integer = 0 : concatMap ( \x -> [x,-x]) [1..]

diophantine :: LDE2 -> [(Integer, Integer)]
diophantine lde@(LDE2 a b c) =
    case concreteSolution lde of
        Nothing -> []
        Just(x0 ,y0) -> map solution integer
            where
                d = gcd a b
                u = div b d
                v = div a d

                solution:: Integer->(Integer, Integer)
                solution k = (x0 + k*u, y0 -k*v)

prettyPrint :: LDE2 -> String
prettyPrint (LDE2 a b c) = show a ++ ".x " ++signB ++ show (abs b)++ ".y = " ++show c
    where signB = if b>= 0 then "+ " else "- "


parseLHS :: String -> Maybe (Integer, Integer)
parseLHS s = 
    case break (=='.') s of
        (aStr, '.':'x':rest2) -> 
            if null rest2 then Nothing else
            let signChar = head rest2
            in if signChar /= '+' && signChar /= '-'
               then Nothing
               else
                   let bRest = tail rest2
                       (bStr, yPart) = break (=='.') bRest
                   in if yPart /= ".y" || null bStr
                      then Nothing
                      else 
                          Just (strToInteger aStr, 
                                (if signChar == '-' then negate else id) (strToInteger bStr))
        _ -> Nothing

    
strToInteger :: String -> Integer
strToInteger [] = 0
strToInteger ('-':xs) =  -1 * strToInteger xs
strToInteger xs = foldl (\acc c -> acc*10 + toInteger (digitToInt c)) 0 xs

toLDE2 :: String -> Maybe LDE2
toLDE2 s =
    let str = filter (/= ' ') s
        (lhs, rest) = break (=='=') str
    in case rest of
        ('=':rhs) -> 
            case parseLHS lhs of
                Nothing -> Nothing
                Just (a,b) -> Just (LDE2 a b (strToInteger rhs))
        _ -> Nothing

--task 2 
data LDEN = LDEN [Integer] Integer deriving Show
--[a1,a2,...,an] a


