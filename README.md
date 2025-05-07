# Prover

A simple proof assistant in Haskell.
This project is designed to implement natural deduction[^1].

# Features

- [x] propositional logic
- [ ] first-order predicate logic

# Tactics

This proof assistant adopts the tactics from the Suppes–Lemmon notation[^2].
The tactics are as follows:

| Tactic     | Example        | Description                                                                          |
| ---------- | -------------- | ------------------------------------------------------------------------------------ |
| assume     | assume p for q | assume p for goal q (where p is any proposition)                                     |
| suppose    | suppose p      | assume p for RAA (where p is any proposition)                                        |
| andI       | andI p&q       | add p&q to assumptions (where p, q are in the assumptions)                           |
| andEL      | andEL p&q      | add p to assumptions (where p&q is in the assumptions)                               |
| andER      | andER p&q      | add q to assumptions (where p&q is in the assumptions)                               |
| orI        | orI p\|q       | add p\|q to assumptions (where p or q is in the assumptions)                         |
| orE        | or p\|q for r  | split into cases: assuming p and assuming q for r (where p\|q is in the assumptions) |
| impI (cp)  | impI p->q      | add p->q to assumptions (where p, q are in the assumptions)                          |
| impE (mpp) | impE p p->q    | add q to assumptions (where p, p->q are in the assumptions)                          |
| dn         | dn p           | add p to assumptions (where !!p is in the assumptions)                               |
| contra     | contra p !p    | when goal is contradiction, complete the proof (where p, !p are in the assumptions)  |
| done       | done           | complete the proof when the goal is already in the assumptions                       |

| Tactic  | Example              | Description                                                                |
| ------- | -------------------- | -------------------------------------------------------------------------- |
| forallI | forallI x p(x)       | add forall x. p(x) to assumptions (if p(a) is provedf for any a)           |
| forallE | forallE p(a)         | add p(a) to assumptions (where forall x. p(x) is in the assumptions)       |
| existsI | existsI a p(a)       | add exists x. p(x) to assumptions (where p(a) is in the assumptions)       |
| existsE | existsE a p(a) for q | add p(a) for q to assumptions (where exists x. p(x) is in the assumptions) |

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
assume p for p
Goal: (p -> p)
Assumptions: [(p -> p)]
Tactics: [assume p for p]
  Goal: p
  Assumptions: [p]
  Tactics: []


Enter a step:
done
Goal: (p -> p)
Assumptions: [(p -> p)]
Tactics: [assume p for p]


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
