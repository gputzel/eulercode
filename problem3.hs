prime_factors_of :: Integer -> [Integer]
prime_factors_of 0 = []
prime_factors_of 1 = []
prime_factors_of n = f : prime_factors_of(n `div` f)
    where
        f = head $ filter (\d -> n `mod` d == 0) [2..n]

problem3 = maximum $ prime_factors_of 600851475143

-- Function that acts as a random number generator,
-- producing a sequence of numbers between 0 and n - 1
f :: Integer -> Integer -> Integer
f n x = (x^2 +1 ) `mod` n

mapper :: (Integer -> Integer) -> (Integer -> Integer) -> (Integer, Integer) -> (Integer, Integer)
mapper g h (a,b) = (g a, h b)

floyd :: Integer -> (Integer, Integer) -> (Integer, Integer)
floyd n = mapper g (g.g)
    where
        g = f n

pairGCD :: Integer -> Integer -> Integer
pairGCD m 0 = m
pairGCD 0 m = m
pairGCD m n
    | n > m = pairGCD n m
    | otherwise = pairGCD n remainder
        where
            remainder = m - (m `div` n)*n 

absDiff :: Integer -> Integer -> Integer
absDiff m n = (max m n) - (min m n)

getFactor :: Integer -> Integer -> Integer
getFactor n start = head $ filter (1 /=) $ tail $ map diffGCD $ iterate (floyd n) (start, start)
    where
        diffGCD (a,b) = pairGCD n (absDiff a b)

powerMod :: Integer -> Integer -> Integer -> Integer
powerMod n 0 b = 0
powerMod n a 0 = 1
powerMod n 1 b = 1
powerMod n a b
    | odd b = a*r*r `mod` n
    | otherwise = r*r `mod` n
        where
            r = powerMod n a (b `div` 2)

fermatTest :: Integer -> [Integer] 
fermatTest n = map f [1..n-1]
    where
        f a = powerMod n a (n-1)
