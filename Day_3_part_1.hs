import Data.List
import Data.List.Split
import qualified Data.Set as S

type Vector = (Integer, Integer)

main :: IO ()
main = interact solution

solution :: String -> String
solution = show . answer . lines

answer = S.findMin . S.map distance . foldl1 S.intersection .
  map (S.fromList . scanl1 addVector . concatMap parsedVectors . splitOn ",")

distance :: Vector -> Integer
distance (x, y) = abs x + abs y

addVector :: Vector -> Vector -> Vector
addVector (x, y) (a, b) = (x + a, y + b)

parsedVectors :: String -> [Vector]
parsedVectors (x:xs) = take (read xs) $ repeat
  (case x of
      'U' -> (0,  1)
      'D' -> (0, -1)
      'R' -> (1,  0)
      'L' -> (-1, 0))
  
