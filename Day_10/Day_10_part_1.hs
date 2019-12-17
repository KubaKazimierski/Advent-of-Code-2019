import Data.List
import qualified Data.Set as S

type Vector = (Int, Int)

main :: IO ()
main = interact solution

solution :: String -> String
solution = show . maximum 
  . (\m -> map (observableAsteroids m) (allAsteroids m))
  . lines

observableAsteroids :: [String] -> Vector -> Int
observableAsteroids m v =
  (length . S.fromList . map (calculateKey v) . filter (/= v))
  $ allAsteroids m  

calculateKey :: Vector -> Vector -> Double
calculateKey (x, y) (a, b) =
  case r of
    (rx, ry) -> atan2 (fromIntegral rx) (fromIntegral ry)
  where r = (x - a, y - b)

allAsteroids :: [String] -> [Vector]
allAsteroids m = [(x, y) |
                  x <- [0..(length (head m) - 1)],
                  y <- [0..(length m - 1)],
                  ((m!!y)!!x) == '#']
