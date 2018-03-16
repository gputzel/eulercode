--This is an implementation of the Euclidean algorithm
--to find the greatest common divisor of two integers, m and n
--It runs in O(log(max(m,n))) time: see
--https://en.wikipedia.org/wiki/Euclidean_algorithm#Algorithmic_efficiency
pairGCD :: Integer -> Integer -> Integer
pairGCD m 0 = m
pairGCD 0 m = m
pairGCD m n
    | n > m = pairGCD n m
    | otherwise = pairGCD n remainder
        where
            remainder = m - (m `div` n)*n 

--This uses the fact that GCD(m,n)*LCM(m,n) = mn
--to reduce the calculation of the LCM of a pair of
--integers to the calculation of their GCD. Hence
--the LCM has the same time complexity, O(log(max(m,n)))
pairLCM :: Integer -> Integer -> Integer
pairLCM m n = (m*n) `div` (pairGCD m n)

--Uses the fact that LCM (a,b,c) = LCM(a,LCM(b,c))
listLCM :: [Integer] -> Integer
listLCM = foldr pairLCM 1
