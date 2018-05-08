isPrime :: Int -> Bool
isPrime a = isPrimeHelper a primes

isPrimeHelper :: Int -> [Int] -> Bool
isPrimeHelper a (p:ps)
        | p*p > a        = True
        | a `mod` p == 0 = False
        | otherwise      = isPrimeHelper a ps

primes = 2 : filter isPrime [3..]

primeFactorization :: Int -> [[Int]]
primeFactorization 1 = []
primeFactorization n = primeFactorizationHelp n primes

maxPow :: Int -> Int -> Int
maxPow p n = tooBig - 1
    where
	tooBig = head $ filter (\k -> n `mod` p^k /= 0) [0..]

primeFactorizationHelp :: Int -> [Int] -> [[Int]]
primeFactorizationHelp n (p:ps)
    | n == 1 = []
    | p*p > n	= [[n,1]]
    | (n `mod` p == 0) = [p,k]:(primeFactorizationHelp (quot n (p^k)) ps)
    | otherwise = primeFactorizationHelp n ps
    where k = maxPow p n

intSqrt = floor . sqrt . fromIntegral

numDivisors :: Int -> Int
numDivisors n = product $ map (\l -> 1 + l!!1) (primeFactorization n)

triangle :: Int -> Int
triangle n = quot (n*(n+1)) 2

numDivisorsOfNthTriangle :: Int -> Int
numDivisorsOfNthTriangle n
    | even n = (numDivisors (quot n 2))*(numDivisors (n+1))
    | otherwise = (numDivisors n)*(numDivisors (quot (n+1) 2))

problem12 :: Int -> Int
problem12 thresh = triangle n
    where
	n = head $ filter (\k -> numDivisorsOfNthTriangle k > thresh) [1..]
