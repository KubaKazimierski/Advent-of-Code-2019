import Data.List
import Data.List.Split

data Instruction = ARG2 (Int -> Int -> Int) Mode Mode Mode
                 | CJUMP (Int -> Bool) Mode Mode
                 | INPUT Mode
                 | OUTPUT Mode
                 | RELA Mode
                 | HALT

data Mode = POSITION
          | RELATIVE
          | IMMEDIATE
  deriving Show

data Machine = Machine { tape     :: [Int]
                       , input    :: [Int]
                       , output   :: [Int]
                       , position :: Int
                       , offset   :: Int
                       }

main :: IO ()
main = interact solution

solution :: String -> String
solution = intercalate "," . map show . answer . map read . splitOn ","

answer :: [Int] -> [Int]
answer t = output (runMachine
                   (Machine { tape     = t ++ [0..]
                            -- part 1: , input    = [1]
                            , input    = [2]
                            , output   = []
                            , position = 0
                            , offset   = 0}))

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
    decodedInstruction' 1 ms = ARG2 (+) (ms!!0) (ms!!1) (ms!!2)
    decodedInstruction' 2 ms = ARG2 (*) (ms!!0) (ms!!1) (ms!!2)
    decodedInstruction' 3 ms = INPUT  (ms!!0)
    decodedInstruction' 4 ms = OUTPUT (ms!!0)
    decodedInstruction' 5 ms = CJUMP (/= 0) (ms!!0) (ms!!1)
    decodedInstruction' 6 ms = CJUMP (== 0) (ms!!0) (ms!!1)
    decodedInstruction' 7 ms = ARG2 (\a b -> if a < b then 1 else 0) (ms!!0) (ms!!1) (ms!!2)
    decodedInstruction' 8 ms = ARG2 (\a b -> if a == b then 1 else 0) (ms!!0) (ms!!1) (ms!!2)
    decodedInstruction' 9 ms = RELA (ms!!0)
    decodedInstruction' 99 _ = HALT

decodedMode :: Char -> Mode
decodedMode '0' = POSITION
decodedMode '1' = IMMEDIATE
decodedMode '2' = RELATIVE

performInstruction :: Instruction -> Machine -> Machine
performInstruction (ARG2 f m1 m2 m3) m =
  m { tape = changedElement
             (valuePos m3 (p + 3) m)
             ((argValue m1 (p + 1) m)
               `f` (argValue m2 (p + 2) m))
             (tape m)
    , position = p + 4
    }
  where p = (position m)
performInstruction (CJUMP cond m1 m2) m =
  m { position =
      if cond (argValue m1 (p + 1) m) then
        (argValue m2 (p + 2) m)
      else
        p + 3
    }
  where p = (position m)
performInstruction (INPUT m1) m =
  m { tape = changedElement
             (valuePos m1 (p + 1) m)
             (head (input m))
             (tape m)
    , input = tail (input m)
    , position = p + 2
    }
  where p = position m
performInstruction (OUTPUT m1) m = 
  m { output = (argValue m1 (p + 1) m):(output m)
    , position = p + 2
    }
  where p = (position m)
performInstruction (RELA m1) m =
  m { offset = (offset m) + (argValue m1 (p + 1) m)
    , position = p + 2
    }
  where p = (position m)

valuePos :: Mode -> Int -> Machine -> Int
valuePos POSITION p m  = (tape m)!!p
valuePos RELATIVE p m  = ((tape m)!!p + (offset m))
valuePos IMMEDIATE p _ = p

argValue :: Mode -> Int -> Machine -> Int
argValue mode p m = (tape m)!!(valuePos mode p m)

changedElement :: Int -> a -> [a] -> [a]
changedElement p e xs = (take p xs) ++ e:(drop (p + 1) xs)
