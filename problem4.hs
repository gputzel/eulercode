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
--But d = 5 take a while
problem4 :: Integer -> Integer
problem4 d = head $ filter (isProductOfTwo d) (palindromes d)

isProductOfTwo :: Integer -> Integer -> Bool
isProductOfTwo d n = any isGood dDigitNumbers
    where
        dDigitNumbers = [start..finish]
        start = 10^(d-1)
        finish = 10^d - 1
        isGood k = (n `mod` k == 0) && (n `div` k > start-1) && (n `div` k < finish + 1)

--The key is to speed up isProductOfTwo
--This one does d = 5 very fast 
problem4OnceMore :: Integer -> Integer
problem4OnceMore d = head $ filter (isProductOfTwoFast d) (palindromes d)

isProductOfTwoFast :: Integer -> Integer -> Bool
isProductOfTwoFast d n
    | n > maxN = False
    | n < minN = False
    | otherwise = any divides candidates
        where
            maxN = (10^d - 1)^2
            minN = (10^(d-1))^2
            candidates = takeWhile (\k -> (k^2 <= n)) [bottom,(bottom+1)..]
            bottom = n `div` (10^d - 1)
            divides k = ((n `mod` k == 0) && (n `div` k <= (10^d-1)) && (n `div` k >= 10^(d-1)))

--d = 2: 9009
--d = 3: 906609
--d = 4: 99000099
--d = 5: 9966006699
--d = 6: 999000000999
--d = 7: 99956644665999
--d = 8: 9999000000009999
--d = 9: 999900665566009999
--d = 10: 99999834000043899999
--d = 11: 9999994020000204999999
--d = 12: 999999000000000000999999

main :: IO ()
main = print $ (problem4OnceMore 12)
