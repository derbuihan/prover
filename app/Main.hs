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
      subProofs = [],
      tactics = []
    }

-- main :: IO ()
-- main = do
--   print testState
--   loop testState

testState :: ProofState
testState =
  ProofState
    { goal = Or (Atom "p") (Not (Atom "p")),
      assumptions = [],
      subProofs =
        [ ProofState
            { goal = Falsum,
              assumptions = [Not (Or (Atom "p") (Not (Atom "p")))],
              subProofs =
                [ ProofState
                    { goal = Falsum,
                      assumptions = [Falsum, Not (Atom "p"), Not (Or (Atom "p") (Not (Atom "p")))],
                      subProofs = [],
                      tactics = []
                    }
                ],
              tactics = [Assume (Not (Atom "p"))]
            }
        ],
      tactics = [Assume (Not (Or (Atom "p") (Not (Atom "p"))))]
    }

loop :: ProofState -> IO ()
loop state = do
  print state
  putStrLn "Enter a step: "
  stepInput <- getLine
  let tactic = parseTactic stepInput
      newState = update tactic state
  if isProved newState
    then do
      print newState
      putStrLn "Proof completed successfully!"
    else
      loop newState
