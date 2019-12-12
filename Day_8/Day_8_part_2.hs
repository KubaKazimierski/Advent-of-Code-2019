import Data.List

main :: IO ()
main = interact solution

solution :: String -> String
solution = concat . intersperse "\n". splitBy 25
  . map (\w -> if w == 1 then '#' else ' ')
  . foldr (\a acc -> if null acc then a else [if (a!!i) == 2 then (acc!!i) else (a!!i) | i <- [0..(length a - 1)]]) []
  . map (map (read . (:[]))) .  filter (/= "\n") . splitBy (25 * 6)

splitBy :: Int -> [a] -> [[a]]
splitBy s [] = [] 
splitBy s a  = (take s a):(splitBy s (drop s a))
