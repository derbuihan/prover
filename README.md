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

# Tactics

| Tactic     | example            | Description                                                                |
| ---------- | ------------------ | -------------------------------------------------------------------------- |
| assume     | assume x           | add x to assumptions (where x is any prop)                                 |
| andI       | andI p & q         | add p & q to assumptions (where p, q are in the assumptions)               |
| andEL      | andEL p & q        | add p to assumptions (where p & q is in the assumptions)                   |
| andER      | andER p & q        | add q to assumptions (where p & q is in the assumptions)                   |
| orI        | orI p\|q           | and p \| q to assumptions (where p or q is in the assumptions)             |
| orE        | orE p\|q p->r q->r | add r to assumptions (where p \| q, p -> r, q -> r are in the assumptions) |
| impI (cp)  | impI p->q          | add p -> q to assumptions (where p, q are in the assumptions)              |
| impE (mpp) | impE p p->q        | add q to assumptions (where p, p -> q are in the assumptions)              |
| dn         | dn p               | add p to assumptions (where ¬¬p is in the assumptions)                     |
| contra     | contra p !p        | contradiction (where p, ¬p are in the assumptions)                         |
| done       | done               | complete the proof                                                         |
