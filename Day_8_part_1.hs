import Data.List

main :: IO ()
main = interact solution

solution :: String -> String
solution = show
  . (\s -> length (filter (== '1') s) * length (filter (== '2') s))
  . foldr (\a acc -> if length a > length acc then a else acc) []
  . map (filter (/= '0')) . splitBy (25 * 6)

splitBy :: Int -> [a] -> [[a]]
splitBy s [] = [] 
splitBy s a  = (take s a):(splitBy s (drop s a))
