isSquare :: Int -> Bool
isSquare n = truncate(sqrt(x)) * truncate(sqrt(x)) == n
             where x = fromIntegral n

problem9 = [a*b*(truncate $ sqrt $ fromIntegral (b^2-a^2)) | b <- [1..1000], a <- [1..b-1],isSquare (b^2-a^2),a + b + (truncate $ sqrt $ fromIntegral (b^2-a^2)) == 1000 ]

--From gauchopuro, much simpler
problem9better = product . head $ [[a, b, c] |
      a <- [1..1000], b <- [1..1000], c <- [1000 - a - b],
      a ^ 2 + b ^ 2 == c ^ 2]
