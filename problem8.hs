import System.IO
import Data.Char

digitProduct :: String -> Int
digitProduct s = product [digitToInt c | c <- s]

substrings s n = [take n $ drop k s | k <- [0..length(s)-n] ]

main = do
    s <- readFile "problem8data.txt"
    let result = maximum [digitProduct sub | sub <- substrings (init s) 13]
    print result
