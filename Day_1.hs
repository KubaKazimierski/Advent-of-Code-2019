main :: IO ()
main = interact solution

solution :: String -> String
solution = show . sum . map answer . map read . lines

answer :: Integer -> Integer
answer m = (floor (fromIntegral m / 3)) - 2
