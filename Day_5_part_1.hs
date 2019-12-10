import Data.List
import Data.List.Split

data Instruction = ARG2 (Int -> Int -> Int) Mode Mode Mode
                 | INPUT Mode
                 | OUTPUT Mode
                 | HALT

data Mode = POSITION | IMMEDIATE

data Machine = Machine { tape :: [Int]
                       , input :: [Int]
                       , output :: [Int]
                       }
               deriving Show

main :: IO ()
main = interact solution

solution :: String -> String
solution = intercalate "," . map show . answer . map read . splitOn ","

answer :: [Int] -> [Int]
answer t = output (runMachine 0 (Machine { tape=t, input=[1..], output=[] }))

runMachine :: Int -> Machine -> Machine
runMachine p m = if halt then m
                 else runMachine (p + instructionOffset cinstruction) (performInstruction cinstruction p m)
  where
    cinstruction = decodedInstruction ((tape m)!!p)
    halt         = case cinstruction of { HALT -> True; _ -> False }

decodedInstruction :: Int -> Instruction
decodedInstruction input = decodedInstruction' instruction modes 
  where
    instruction = read $ drop (length ninput - 2) ninput
    modes       = map decodedMode $ drop 2 $ reverse ninput
    ninput      = (take (5 - (length . show) input) $ repeat '0') ++ (show input)
    
    decodedInstruction' :: Int -> [Mode] -> Instruction
    decodedInstruction' 1 ms = ARG2 (+) (ms!!0) (ms!!1) (ms!!2)
    decodedInstruction' 2 ms = ARG2 (*) (ms!!0) (ms!!1) (ms!!2)
    decodedInstruction' 3 ms = INPUT (ms!!0)
    decodedInstruction' 4 ms = OUTPUT (ms!!0)
    decodedInstruction' 99 _ = HALT

decodedMode :: Char -> Mode
decodedMode '0' = POSITION
decodedMode '1' = IMMEDIATE

performInstruction :: Instruction -> Int -> Machine -> Machine
performInstruction (ARG2 f m1 m2 _) p m =
  m { tape = changedElement
             ((tape m)!!(p + 3))
             ((getArgValue m1 (p + 1) (tape m))
               `f` (getArgValue m2 (p + 2) (tape m)))
             (tape m)
    }
performInstruction (INPUT _) p m =
  m { tape = changedElement
             ((tape m)!!(p + 1))
             (head (input m))
             (tape m)
    , input = tail (input m)
    }
performInstruction (OUTPUT m1) p m =
    m { output = (getArgValue m1 (p + 1) (tape m)):(output m) }

getArgValue :: Mode -> Int -> [Int] -> Int
getArgValue POSITION p t = t!!(t!!p)
getArgValue IMMEDIATE p t = t!!p

changedElement :: Int -> a -> [a] -> [a]
changedElement p e xs = (take p xs) ++ e:(drop (p + 1) xs)

instructionOffset :: Instruction -> Int
instructionOffset (ARG2 _ _ _ _) = 4
instructionOffset (INPUT _)      = 2
instructionOffset (OUTPUT _)     = 2
instructionOffset HALT           = 1
