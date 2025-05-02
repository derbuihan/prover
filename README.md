# Prover

A simple proof assistant in Haskell.

# Usage

```bash
$ cabal run
Welcome to the Prover!
Enter a goal:
p|!p
Enter assumptions (separated by commas):

Goal: (p | !p)
Assumptions: []
Tactics: []

Enter a step:
assume !(p|!p)
Goal: (p | !p)
Assumptions: [!!(p | !p)]
Tactics: [assume !(p | !p)]
  Goal: ⊥
  Assumptions: [!(p | !p)]
  Tactics: []


Enter a step:
assume !p
Goal: (p | !p)
Assumptions: [!!(p | !p)]
Tactics: [assume !(p | !p)]
  Goal: ⊥
  Assumptions: [!!p,!(p | !p)]
  Tactics: [assume !p]
    Goal: ⊥
    Assumptions: [!p,!(p | !p)]
    Tactics: []



Enter a step:
orI p|!p
Goal: (p | !p)
Assumptions: [!!(p | !p)]
Tactics: [assume !(p | !p)]
  Goal: ⊥
  Assumptions: [!!p,!(p | !p)]
  Tactics: [assume !p]
    Goal: ⊥
    Assumptions: [(p | !p),!p,!(p | !p)]
    Tactics: [orI (p | !p)]



Enter a step:
contra p|!p !(p|!p)
Goal: (p | !p)
Assumptions: [!!(p | !p)]
Tactics: [assume !(p | !p)]
  Goal: ⊥
  Assumptions: [!!p,!(p | !p)]
  Tactics: [assume !p]
    Goal: ⊥
    Assumptions: [⊥,(p | !p),!p,!(p | !p)]
    Tactics: [contra (p | !p) !(p | !p),orI (p | !p)]



Enter a step:
done
Goal: (p | !p)
Assumptions: [!!(p | !p)]
Tactics: [assume !(p | !p)]
  Goal: ⊥
  Assumptions: [!!p,!(p | !p)]
  Tactics: [assume !p]



Enter a step:
dn !!p
Goal: (p | !p)
Assumptions: [!!(p | !p)]
Tactics: [assume !(p | !p)]
  Goal: ⊥
  Assumptions: [p,!!p,!(p | !p)]
  Tactics: [dn !!p,assume !p]



Enter a step:
orI p|!p
Goal: (p | !p)
Assumptions: [!!(p | !p)]
Tactics: [assume !(p | !p)]
  Goal: ⊥
  Assumptions: [(p | !p),p,!!p,!(p | !p)]
  Tactics: [orI (p | !p),dn !!p,assume !p]



Enter a step:
contra p|!p !(p|!p)
Goal: (p | !p)
Assumptions: [!!(p | !p)]
Tactics: [assume !(p | !p)]
  Goal: ⊥
  Assumptions: [⊥,(p | !p),p,!!p,!(p | !p)]
  Tactics: [contra (p | !p) !(p | !p),orI (p | !p),dn !!p,assume !p]



Enter a step:
done
Goal: (p | !p)
Assumptions: [!!(p | !p)]
Tactics: [assume !(p | !p)]


Enter a step:
dn !!(p|!p)
Goal: (p | !p)
Assumptions: [(p | !p),!!(p | !p)]
Tactics: [dn !!(p | !p),assume !(p | !p)]


Enter a step:
done

Proof completed successfully!
```

# Tactics

| Tactic     | example            | Description                                                                         |
| ---------- | ------------------ | ----------------------------------------------------------------------------------- |
| assume     | assume x           | add x to assumptions (where x is any prop)                                          |
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
