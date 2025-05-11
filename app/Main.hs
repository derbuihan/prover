module Main where

import Parser
import Prove
import Types

-- main :: IO ()
-- main = do
--   putStrLn "Enter a prop:"
--   input <- getLine
--   let tokens = tokenize input
--       (parsedProp, _) = parseProp_ tokens
--       convertedProp = convertProp parsedProp
--   putStrLn $ "Tokens: " ++ show tokens
--   putStrLn $ "Parsed Prop: " ++ show parsedProp
--   putStrLn $ "Converted Prop: " ++ show convertedProp

main :: IO ()
main = do
  putStrLn "Welcome to the Prover!"
  putStrLn "Enter a goal: "
  goalInput <- getLine
  putStrLn "Enter assumptions (separated by commas): "
  assumptionsInput <- getLine
  let parsedGoal = parseProp goalInput
      parsedAssum = parseAssumptions assumptionsInput
  loop $ initState parsedGoal parsedAssum

initState :: Prop -> [Prop] -> ProofState
initState parsedGoal parsedAssum =
  ProofState
    { goal = parsedGoal,
      assumptions = parsedAssum,
      subProofs = [],
      tactics = [],
      completed = False
    }

loop :: ProofState -> IO ()
loop state = do
  print state
  putStrLn "Enter a step: "
  stepInput <- getLine
  let tactic = parseTactic stepInput
      newState = prove tactic state
  if completed newState
    then do
      print newState
      putStrLn "Proof completed successfully!"
    else
      loop newState
