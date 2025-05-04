module Main where

import Parser
import Prove
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
      subProofs = [],
      tactics = []
    }

loop :: ProofState -> IO ()
loop state = do
  print state
  putStrLn "Enter a step: "
  stepInput <- getLine
  let tactic = parseTactic stepInput
      newState = prove tactic state
  if isProved newState
    then do
      print newState
      putStrLn "Proof completed successfully!"
    else
      loop newState
