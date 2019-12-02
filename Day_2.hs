import Data.List
import Data.List.Split

main :: IO ()
main = interact solution

solution :: String -> String
solution = intercalate "," . map show . answer . map read . splitOn ","

answer :: [Int] -> [Int]
answer = runMachine 0

runMachine :: Int -> [Int] -> [Int]
runMachine _ [] = []
runMachine p xs
  | xs!!p == 99 = xs
  | otherwise   =
      runMachine (p + 4) result
    where args = [xs!!(xs!!(p + 1)), xs!!(xs!!(p + 2))]
          result =
            let (s, e) = splitAt (xs!!(p + 3) + 1) xs in
              init s ++ ((if xs!!p == 1 then sum else product) args):e
            
