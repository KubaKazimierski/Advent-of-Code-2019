import Data.List
import Data.List.Split

main :: IO ()
main = getLine >>=
  (return . solution . map read . splitOn "-") >>=
  putStrLn

solution :: [Integer] -> String
solution xs = show $ length [x | x <- [xs!!0..xs!!1], condition x]

condition :: Integer -> Bool
condition = (\v -> isIncreasing v && isAdjacentPair v) . show

isAdjacentPair :: [Char] -> Bool
isAdjacentPair [] = False
isAdjacentPair xs = length (filter (== (xs!!0)) xs) == 2 || isAdjacentPair (filter (/= (xs!!0)) xs)

isIncreasing :: [Char] -> Bool
isIncreasing []       = True
isIncreasing [x]      = True
isIncreasing (a:b:xs) = a <= b && isIncreasing (b:xs)
