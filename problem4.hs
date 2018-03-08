digitsReversed :: Integer -> [Integer]
digitsReversed n = spam n
    where
        spam n
            | n < 10 = [n]
            | otherwise = (n `mod` 10):spam(n `div` 10)

isPalindrome :: Integer -> Bool
isPalindrome n = (==) (digitsReversed n) (reverse $ digitsReversed n)

--19.41 seconds on my machine in GHCI
--12.38 seconds after I stopped wastefully reversing a list in the digitsOf function
problem4Slow = maximum $ filter isPalindrome [n*m | n <- [100..999], m <- [100..999]]

--8.64 seconds
problem4Better = maximum $ map head' [filter isPalindrome [n*m | n <- [999,998..1]] | m <- [999,998..1]]
    where
        head' [] = -1
        head' (x:xs) = x

--7.91 seconds
problem4TryAgain = maximum $ map f [999,998..1]
    where
        f n = head' $ filter isPalindrome $ map (n*) [999,998..1]
        head' [] = -1
        head' (x:xs) = x

--Enough of this foolishness. Just enumerate palindromes in reverse order
--and pick the first one that's small enough to be the product of
--two three-digit numbers
palindromes :: Integer -> [Integer]
palindromes halfDigits = map makePalindrome [max,(max-1)..1]
    where
        max = 10^(halfDigits) - 1
        makePalindrome n = (max+1)*n + (fromDigits (digitsReversed n))

--I admit, I tried foldr first
fromDigits :: [Integer] -> Integer
fromDigits = foldl (\a b -> 10*a + b) 0

--For d = 3, takes 0.08 seconds
problem4 :: Integer -> Integer
problem4 d = head $ filter (isProductOfTwo d) (palindromes d)

isProductOfTwo :: Integer -> Integer -> Bool
isProductOfTwo d n = any isGood dDigitNumbers
    where
        dDigitNumbers = [start..finish]
        start = 10^(d-1)
        finish = 10^d - 1
        isGood k = (n `mod` k == 0) && (n `div` k > start-1) && (n `div` k < finish + 1)
