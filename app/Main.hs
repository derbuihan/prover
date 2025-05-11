module Main where

import Convert
import Prove
import Types

main :: IO ()
main = do
  putStrLn "Welcome to the Prover!"
  putStrLn "Enter a goal: "
  goalInput <- getLine
  putStrLn "Enter assumptions (separated by commas): "
  assumptionsInput <- getLine
  let parsedGoal = parseAndConvertProp goalInput
      parsedAssum = parseAndConvertAssumptions assumptionsInput
  loop $ initState parsedGoal parsedAssum

initState :: Prop -> [Prop] -> ProofState
initState parsedGoal parsedAssum =
  ProofState
    { goal = parsedGoal,
      assumptions = parsedAssum,
      subProofs = [],
      tactics = [],
      completed = False,
      fixed = []
    }

loop :: ProofState -> IO ()
loop state = do
  print state
  putStrLn "Enter a step: "
  stepInput <- getLine
  let tactic = parseAndConvertTactic stepInput
      newState = prove tactic state
  if completed newState
    then do
      print newState
      putStrLn "Proof completed successfully!"
    else
      loop newState
