import Data.List
import qualified Data.Set as S

type Vector = (Int, Int)
data Key = ANGLE Double | ITSELF
  deriving (Eq, Ord)

main :: IO ()
main = interact solution

solution :: String -> String
solution = show . maximum 
  . (\m -> map (observableAsteroids m) (allAsteroids m))
  . lines

observableAsteroids :: [String] -> Vector -> Int
observableAsteroids m v =
  (length . S.filter (/= ITSELF) . S.fromList . map (calculateKey v))
  $ allAsteroids m  

calculateKey :: Vector -> Vector -> Key
calculateKey (x, y) (a, b) =
  case r of
    (0, 0)   -> ITSELF
    (rx, ry) -> ANGLE (atan2 (fromIntegral rx) (fromIntegral ry))
  where r = (x - a, y - b)

allAsteroids :: [String] -> [Vector]
allAsteroids m = [(x, y) |
                  x <- [0..(length m - 1)],
                  y <- [0..(length (head m) - 1)],
                  ((m!!y)!!x) == '#']
