# Prover

A simple proof assistant in Haskell.
This project is designed to implement natural deduction[^1].

# Features

- [x] propositional logic
- [ ] first-order predicate logic

# Tactics

This proof assistant adopts the tactics from the Suppes–Lemmon notation[^2].
The tactics are as follows:

| Tactic     | example     | Description                                                                         |
| ---------- | ----------- | ----------------------------------------------------------------------------------- |
| assume     | assume x    | assume x for RAA (where x is any proposition)                                       |
| andI       | andI        | split the goal p & q into two subgoals p and q (where p & q is the goal)            |
| andEL      | andEL p & q | add p to assumptions (where p & q is in the assumptions)                            |
| andER      | andER p & q | add q to assumptions (where p & q is in the assumptions)                            |
| orIL       | orIL        | choose p as the left disjunct (where p \| q is the goal)                            |
| orIR       | orIR        | choose q as the right disjunct (where p \| q is the goal)                           |
| orE        | orE p\|q    | split into cases: assuming p and assuming q (where p \| q is in the assumptions)    |
| impI (cp)  | impI        | add p to assumptions and change the goal to q (where p -> q is the goal)            |
| impE (mpp) | impE p p->q | add q to assumptions (where p, p -> q are in the assumptions)                       |
| dnI        | dnI         | change the goal to p (where !!p is the goal)                                        |
| dnE        | dnE p       | add p to assumptions (where ¬¬p is in the assumptions)                              |
| contra     | contra p !p | when goal is contradiction, complete the proof (where p, !p are in the assumptions) |
| done       | done        | complete the proof when the goal is already in the assumptions                      |

assume, andI, andEL, andER, orIL, orIR, orE, impI, impE, dnI, dnE, contra, done

# Usage

The following command will run the Prover:

```bash
$ cabal run
Welcome to the Prover!
Enter a goal:
p->p
Enter assumptions (separated by commas):

Goal: (p -> p)
Assumptions: []
Tactics: []

Enter a step:
assume p
Goal: p
Assumptions: [p]
Tactics: [assume p]

Enter a step:
done

Proof completed successfully!
```

Other examples can be found in the `examples` directory.

```bash
$ bash ./example/run.sh
```

[^1]: https://en.wikipedia.org/wiki/Natural_deduction
[^2]: https://en.wikipedia.org/wiki/Suppes%E2%80%93Lemmon_notation
