# Prover

A simple proof assistant in Haskell.
This project is designed to implement [natural deduction][^1].

# Features

- [x] propositional logic
- [ ] first-order predicate logic

# Tactics

This proof assistant adopts the tactics from the [Suppes–Lemmon notation][^2].
The tactics are as follows:

| Tactic     | example            | Description                                                                         |
| ---------- | ------------------ | ----------------------------------------------------------------------------------- |
| assume     | assume x           | add x to assumptions (where x is the antecedent of the goal)                        |
| suppose    | suppose x          | assume x for RAA (where x is any proposition)                                       |
| andI       | andI p & q         | add p & q to assumptions (where p, q are in the assumptions)                        |
| andEL      | andEL p & q        | add p to assumptions (where p & q is in the assumptions)                            |
| andER      | andER p & q        | add q to assumptions (where p & q is in the assumptions)                            |
| orI        | orI p\|q           | and p \| q to assumptions (where p or q is in the assumptions)                      |
| orE        | orE p\|q p->r q->r | add r to assumptions (where p \| q, p -> r, q -> r are in the assumptions)          |
| impI (cp)  | impI p->q          | add p -> q to assumptions (where p, q are in the assumptions)                       |
| impE (mpp) | impE p p->q        | add q to assumptions (where p, p -> q are in the assumptions)                       |
| dn         | dn p               | add p to assumptions (where ¬¬p is in the assumptions)                              |
| contra     | contra p !p        | when goal is contradiction, complete the proof (where p, !p are in the assumptions) |
| done       | done               | complete the proof                                                                  |

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

Other examples can be found in the examples directory.

```bash
$ bash ./example/run.sh
```

[^1]: https://en.wikipedia.org/wiki/Natural_deduction
[^2]: https://en.wikipedia.org/wiki/Suppes%E2%80%93Lemmon_notation
