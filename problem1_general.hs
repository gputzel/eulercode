--Notations for time complexity:
--The problem is to sum up all the integers from 1 to (but not including) N,
--which are divisible by any of a set of M integers. Let the largest of those M
--integers be called K.

-- For the purposes of testing, here is a simple solution
-- of the "generalized problem 1".
-- This solution takes O(NM) time.
multipleSumSimple :: [Int] -> Int -> Int
multipleSumSimple l n = sum $ filter anyDivides [1..(n-1)]
    where
        anyDivides m = or [m `mod` k == 0 | k <- l]

--This function returns the sum of all integers from 1 to n-1
--that are divisible by k. This takes O(1) time, as there
--are no loops or anything like that.
sumMultiplesUpTo :: Int -> Int -> Int
sumMultiplesUpTo k n = k*s
    where
        s = q*(q+1) `div` 2
        q = (n-1) `div` k

--This is an implementation of the Euclidean algorithm
--to find the greatest common divisor of two integers, m and n
--It runs in O(log(max(m,n))) time: see
--https://en.wikipedia.org/wiki/Euclidean_algorithm#Algorithmic_efficiency
pairGCD :: Int -> Int -> Int
pairGCD m 0 = m
pairGCD 0 m = m
pairGCD m n
    | n > m = pairGCD n m
    | otherwise = pairGCD n remainder
        where
            remainder = m - (m `div` n)*n 

--Calculating the GCD of a list of integers can be reduced to
--calculating GCDs of pairs of integers. For a list of M integers
--of size at most K, this will take O(M*log K) time.
--Note: we won't actually be using this function
listGCD :: [Int] -> Int
listGCD (a:[]) = a
listGCD (a:l) = pairGCD a $ listGCD l

--This uses the fact that GCD(m,n)*LCM(m,n) = mn
--to reduce the calculation of the LCM of a pair of
--integers to the calculation of their GCD. Hence
--the LCM has the same time complexity, O(log(max(m,n)))
pairLCM :: Int -> Int -> Int
pairLCM m n = (m*n) `div` (pairGCD m n)

--As with the GCD, the LCM of a list of M integers can
--be reduced to LCM calculations for pairs. If the largest
--integer in the list is K, then this runs in time
--O(M*log K) 
listLCM :: [Int] -> Int
listLCM (a:[]) = a
listLCM (a:l) = pairLCM a $ listLCM l

--This function produces a list of lists:
--namely a list containing all the subsets (viewed as lists)
--of [1..n]
--At best, this can be implemented in time O(2^n)
--and I assume this is how fast this solution runs
subsetsOfSize :: Int -> [Int] -> [[Int]]
subsetsOfSize 0 l = [[]]
subsetsOfSize _ [] = []
subsetsOfSize k (a:l) = subsetsWith ++ subsetsWithout
    where
        subsetsWith = [a:s | s <- subsetsOfSize (k-1) l]
        subsetsWithout = [s | s <- subsetsOfSize k l]

--This function uses the "Inclusion-Exclusion Principle" to calculate what we want. See
--https://en.wikipedia.org/wiki/Inclusion%E2%80%93exclusion_principle#Counting_integers
--This principle reduces the desired sum to a sum over all subsets of the input set of integers.
--For each subset, we're applying the sumMultiplesUpTo function with all multiples of the LCM of the subset
--The time complexity is O(log(K)*M*2^M)
multipleSum :: [Int] -> Int -> Int
multipleSum l n = sum $ zipWith (*) signs terms
    where
        signs = 1:(-1:signs)
        terms = map termsWith [1..(length l)]
        termsWith k = sum [sumMultiplesUpTo (listLCM subset) n | subset <- (subsetsOfSize k l)]
