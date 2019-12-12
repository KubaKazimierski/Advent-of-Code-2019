import Data.List
import Data.List.Split

data Instruction = ARG2 (Int -> Int -> Int) Mode Mode Mode
                 | CJUMP (Int -> Bool) Mode Mode 
                 | INPUT Mode
                 | OUTPUT Mode
                 | HALT

data Mode = POSITION | IMMEDIATE

data Machine = Machine { tape     :: [Int]
                       , input    :: [Int]
                       , output   :: [Int]
                       , position :: Int
                       }
               deriving Show

main :: IO ()
main = interact solution

solution :: String -> String
solution = show . answer . map read . splitOn ","

answer :: [Int] -> Int
answer t = maximum . amplifiers $ Machine { tape=t, input=[], output=[], position=0 } 

amplifiers :: Machine -> [Int]
amplifiers m = map (foldr runAmplifier 0 . (map (\i -> m { input=[i] }))) $ permutations [0..4]
  where
    runAmplifier :: Machine -> Int -> Int
    runAmplifier a i = head . output $ runMachine (a { input = (input a) ++ [i] }) 

runMachine :: Machine -> Machine
runMachine m = if halt then m
               else runMachine (performInstruction cinstruction m)
  where
    cinstruction = decodedInstruction ((tape m)!!(position m))
    halt         = case cinstruction of { HALT -> True; _ -> False }

decodedInstruction :: Int -> Instruction
decodedInstruction input = decodedInstruction' instruction modes 
  where
    instruction = read $ drop (length ninput - 2) ninput
    modes       = map decodedMode $ drop 2 $ reverse ninput
    ninput      = (take (5 - (length . show) input) $ repeat '0') ++ (show input)
    
    decodedInstruction' :: Int -> [Mode] -> Instruction
    decodedInstruction' 1 ms = ARG2 (+) (ms!!0) (ms!!1) POSITION
    decodedInstruction' 2 ms = ARG2 (*) (ms!!0) (ms!!1) POSITION
    decodedInstruction' 3 _  = INPUT POSITION
    decodedInstruction' 4 ms = OUTPUT (ms!!0)
    decodedInstruction' 5 ms = CJUMP (/= 0) (ms!!0) (ms!!1)
    decodedInstruction' 6 ms = CJUMP (== 0) (ms!!0) (ms!!1)
    decodedInstruction' 7 ms = ARG2 (\a b -> if a < b then 1 else 0) (ms!!0) (ms!!1) POSITION
    decodedInstruction' 8 ms = ARG2 (\a b -> if a == b then 1 else 0) (ms!!0) (ms!!1) POSITION
    decodedInstruction' 99 _ = HALT

decodedMode :: Char -> Mode
decodedMode '0' = POSITION
decodedMode '1' = IMMEDIATE

performInstruction :: Instruction -> Machine -> Machine
performInstruction (ARG2 f m1 m2 _) m =
  m { tape = changedElement
             ((tape m)!!(p + 3))
             ((getArgValue m1 (p + 1) (tape m))
               `f` (getArgValue m2 (p + 2) (tape m)))
             (tape m)
    , position = p + 4
    }
  where p = (position m)
performInstruction (CJUMP cond m1 m2) m =
  m { position =
      if cond (getArgValue m1 (p + 1) (tape m)) then
        (getArgValue m2 (p + 2) (tape m))
      else
        p + 3
    }
  where p = (position m)
performInstruction (INPUT _) m =
  m { tape = changedElement
             ((tape m)!!(p + 1))
             (head (input m))
             (tape m)
    , input = tail (input m)
    , position = p + 2
    }
  where p = position m
performInstruction (OUTPUT m1) m =
  m { output = (getArgValue m1 (p + 1) (tape m)):(output m)
    , position = p + 2
    }
  where p = (position m)

getArgValue :: Mode -> Int -> [Int] -> Int
getArgValue POSITION p t = t!!(t!!p)
getArgValue IMMEDIATE p t = t!!p

changedElement :: Int -> a -> [a] -> [a]
changedElement p e xs = (take p xs) ++ e:(drop (p + 1) xs)
