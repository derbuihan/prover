# Prover

A simple proof assistant in Haskell.

# Usage

```bash
$ cabal run
Welcome to the Prover!
Enter a goal:
x->(y->x)
Current proof state:
    Goal: (x -> (y -> x))
    Assumptions: []
    Tactics: []

Enter a step:
assum x
Current proof state:
    Goal: (y -> x)
    Assumptions: [x]
    Tactics: [assum x]

Enter a step:
assum y
Current proof state:
    Goal: x
    Assumptions: [y,x]
    Tactics: [assum y,assum x]

Enter a step:
done
Current proof state:
    Goal: x
    Assumptions: [y,x]
    Tactics: [done,assum y,assum x]

Proof completed successfully!
```
