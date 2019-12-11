import Data.List
import Data.List.Split
import qualified Data.Set as S

main :: IO ()
main = interact solution

solution :: String -> String
solution = show . answer . map ((\[x, y] -> (x, y)) . splitOn ")") . lines

answer :: [(String, String)] -> Integer
answer m = (fromIntegral . (+ (-2)) . S.size . foldr (\x y -> S.union x y S.\\ S.intersection x y) S.empty . orbitPaths m)
  $ filter (\(_, x) -> x == "YOU" || x == "SAN") m

orbitPaths :: [(String, String)] -> [(String, String)] -> [S.Set (String, String)] 
orbitPaths m s = map S.fromList $ map (\(p@(x, y)) -> orbitPath m p) s

orbitPath :: [(String, String)] -> (String, String) -> [(String, String)] 
orbitPath m o@("COM", _) = [o]
orbitPath m o@(x, _)     = o:(orbitPath m $ head $ filter (\(_, a) -> a == x) m)
