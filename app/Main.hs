module Main where

import Parser
import Proof
import Types

main :: IO ()
main = do
  putStrLn "Welcome to the Prover!"
  putStrLn "Enter a goal: "
  goalInput <- getLine
  let parsedGoal = parseProp goalInput
  loop $ initState parsedGoal

initState :: Prop -> ProofState
initState parsedGoal =
  ProofState
    { goal = parsedGoal,
      assumptions = [],
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
