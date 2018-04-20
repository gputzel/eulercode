--From vineet's solution to problem 7:
isPrime a = isPrimeHelper a primes

isPrimeHelper a (p:ps)
        | p*p > a        = True
        | a `mod` p == 0 = False
        | otherwise      = isPrimeHelper a ps

primes = 2 : filter isPrime [3..]

problem10 = sum $ takeWhile (<2000000) primes
