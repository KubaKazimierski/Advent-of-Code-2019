import Data.List
import qualified Data.Set as S
import Debug.Trace

type Vector = (Int, Int)

main :: IO ()
main = interact solution

solution :: String -> String
solution = show
  . (\(v, _) -> v) . last . take 200
  . concat
  . transpose
  . (\as ->
       ((\p ->
            map (sortBy (distanceFrom p))
          $ groupBy (\(_, x) (_, y) -> x == y)
          $ sortBy angles
          $ observableAsteroids as p)
         (stationPosition as)))
  . allAsteroids . lines
  where
    angles (_, x) (_, y) = compare x y
    distanceFrom origin (x, _) (y, _) =
      compare (vecLength $ substract origin x) (vecLength $ substract origin y) 

observableAsteroids :: [Vector] -> Vector -> [(Vector, Double)]
observableAsteroids is v = (\as -> zip as (map (calculateKey v) as))
  $ filter (/= v) is

stationPosition :: [Vector] -> Vector
stationPosition = (\(v, _) -> v) . head
  . sortBy (\(_, x) (_, y) -> compare y x)
  . (\vs -> zip vs (map (visibleAsteroids vs) vs))

visibleAsteroids :: [Vector] -> Vector -> Int
visibleAsteroids m v = (length . S.fromList . map (calculateKey v) . filter (/= v)) m

calculateKey :: Vector -> Vector -> Double
calculateKey a b = angle (substract a b)

allAsteroids :: [String] -> [Vector]
allAsteroids m = [(x, y) |
                  x <- [0..(length (head m) - 1)],
                  y <- [0..(length m - 1)],
                  ((m!!y)!!x) == '#']

substract :: Vector -> Vector -> Vector
substract (x, y) (a, b) = (x - a, y - b)

angle :: Vector -> Double
angle (x, y)
  | a /= 360  = a
  | otherwise = 0
  where a = (atan2 (fromIntegral x) (fromIntegral (-y)) + pi) * 180 / pi

vecLength :: Vector -> Double
vecLength (x, y) = sqrt $ fromIntegral $ (x ^ 2) + (y ^ 2)
