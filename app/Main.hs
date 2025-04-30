module Main where

import Parser
import Proof
import Types

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
      tactics = []
    }

printState :: ProofState -> IO ()
printState state = do
  putStrLn "Current proof state:"
  print state

loop :: ProofState -> IO ()
loop state = do
  printState state
  putStrLn "Enter a step: "
  stepInput <- getLine
  let tactic = parseTactic stepInput
      newState = prove tactic state
  case head (tactics newState) of
    Done -> do
      printState newState
      putStrLn "Proof completed successfully!"
    _ -> loop newState
