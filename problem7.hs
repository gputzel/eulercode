--primeSieve[1..n] gives the primes up to n
primeSieve :: [Integer] -> [Integer]
primeSieve [] = []
primeSieve (1:l) = primeSieve l
primeSieve (p:l) = p:(primeSieve $ filter (\n -> n `mod` p /= 0) l)

problem7 = (primeSieve [1..])!!10000

--vineet's solution, far far better
isPrime a = isPrimeHelper a primes

isPrimeHelper a (p:ps)
        | p*p > a        = True
        | a `mod` p == 0 = False
        | otherwise      = isPrimeHelper a ps

primes = 2 : filter isPrime [3..]

vineetSol =  primes !! 10000
