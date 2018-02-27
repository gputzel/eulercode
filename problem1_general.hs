multipleSumSimple :: [Int] -> Int -> Int
multipleSumSimple l n = sum $ filter anyDivides [1..(n-1)]
    where
        anyDivides m = or [m `mod` k == 0 | k <- l]

sumMultiplesUpTo :: Int -> Int -> Int
sumMultiplesUpTo k n = k*s
    where
        s = q*(q+1) `div` 2
        q = (n-1) `div` k

pairGCD :: Int -> Int -> Int
pairGCD m 0 = m
pairGCD 0 m = m
pairGCD m n
    | n > m = pairGCD n m
    | otherwise = pairGCD n remainder
        where
            remainder = m - (m `div` n)*n 

listGCD :: [Int] -> Int
listGCD (a:[]) = a
listGCD (a:l) = pairGCD a $ listGCD l

pairLCM :: Int -> Int -> Int
pairLCM m n = (m*n) `div` (pairGCD m n)

listLCM :: [Int] -> Int
listLCM (a:[]) = a
listLCM (a:l) = pairLCM a $ listLCM l

subsetsOfSize :: Int -> [Int] -> [[Int]]
subsetsOfSize 0 l = [[]]
subsetsOfSize _ [] = []
subsetsOfSize k (a:l) = subsetsWith ++ subsetsWithout
    where
        subsetsWith = [a:s | s <- subsetsOfSize (k-1) l]
        subsetsWithout = [s | s <- subsetsOfSize k l]

multipleSum :: [Int] -> Int -> Int
multipleSum l n = sum $ zipWith (*) signs terms
    where
        signs = 1:(-1:signs)
        terms = map termsWith [1..(length l)]
        termsWith k = sum [sumMultiplesUpTo (listLCM subset) n | subset <- (subsetsOfSize k l)]
