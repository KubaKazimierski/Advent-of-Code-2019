import Data.List
import Data.List.Split
import qualified Data.Map as M

main :: IO ()
main = interact solution

solution :: String -> String
solution = show . answer . map ((\[x, y] -> (x, y)) . splitOn ")") . lines

answer :: [(String, String)] -> Integer
answer = foldr (+) 0 . orbitPaths

orbitPaths :: [(String, String)] -> M.Map String Integer 
orbitPaths m = M.fromList $ map (\(p@(x, y)) -> (y, orbitPath m p)) m

orbitPath :: [(String, String)] -> (String, String) -> Integer
orbitPath m ("COM", _) = 1
orbitPath m (x, y)     = (+1) $ orbitPath m $ head $ filter (\(_, a) -> a == x) m
