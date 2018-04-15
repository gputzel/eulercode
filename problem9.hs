isSquare :: Int -> Bool
isSquare n = truncate(sqrt(x)) * truncate(sqrt(x)) == n
             where x = fromIntegral n

problem9 = [a*b*(truncate $ sqrt $ fromIntegral (b^2-a^2)) | b <- [1..1000], a <- [1..b-1],isSquare (b^2-a^2),a + b + (truncate $ sqrt $ fromIntegral (b^2-a^2)) == 1000 ]

--From gauchopuro, much simpler
problem9better = product . head $ [[a, b, c] |
      a <- [1..1000], b <- [1..1000], c <- [1000 - a - b],
      a ^ 2 + b ^ 2 == c ^ 2]

problem9general :: Integer -> [[Integer]]
problem9general n = [[a, b, c] |
      a <- [1..n], b <- [1..n], c <- [n - a - b],
      a ^ 2 + b ^ 2 == c ^ 2]

--From Rosetta Code
--https://rosettacode.org/wiki/Factors_of_an_integer#Haskell
factors_o n = ds ++ [r | (d,0) <- [divMod n r], r <- r:[d | d>r]] ++ reverse (map (n `div`) ds)
        where
        r = floor (sqrt (fromIntegral n))
        ds = [i | i <- [1..r-1], mod n i == 0]

problem9_from_factors :: Integer -> [Integer]
problem9_from_factors n = head $ concat $ map triples_from_factor $ factors_o n
    where
        triples_from_factor k = map (scale k) (problem9general k)
        scale k = map (* (n `div` k))
