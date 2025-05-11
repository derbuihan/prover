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

| Tactic  | Example                  | Description                                                                |
| ------- | ------------------------ | -------------------------------------------------------------------------- |
| fix     | fix a                    | fixed a (where a is a variable)                                            |
| forallI | forallI a p(a)           | add forall x. p(x) to assumptions (if p(a) is provedf for any a)           |
| forallE | forallE a forall x. p(x) | add p(a) to assumptions (where forall x. p(x) is in the assumptions)       |
| existsI | existsI a p(a)           | add exists x. p(x) to assumptions (where p(a) is in the assumptions)       |
| existsE | existsE a exists x. p(x) | add p(a) for q to assumptions (where exists x. p(x) is in the assumptions) |

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

# First-order predicate logic

## forall x. p(x) -> exists x. p(x)

```
$ cabal run
Welcome to the Prover!
Enter a goal: exists x. p(x)
Enter assumptions (separated by commas): forall x. p(x)

Goal: (exists x. p(x))
Assumptions: [forall x. p(x)]
Enter a step: forallE a forall x. p(x)

Goal: (exists x. p(x))
Assumptions: [p(a), forall x. p(x)]
Enter a step: existsI p(a) exists x. p(x)

Goal: (exists x. p(x))
Assumptions: [exists x. p(x), p(a), forall x. p(x)]
Enter a step: done
```

## !exists x. p(x) -> forall x. !p(x)

```
$ cabal run
Welcome to the Prover!
Enter a goal: forall x. !p(x)
Enter assumptions (separated by commas): !exists x. p(x)

Goal: (forall x. !p(x))
Assumptions: [!exists x. p(x)]
Tactics: []

Enter a step: suppose p(a)
Goal: (forall x. !p(x))
Assumptions: [p(a), !exists x. p(x)]
Tactics: [suppose p(a)]

Enter a step: existsI p(a) exists x. p(x)
Goal: (forall x. !p(x))
Assumptions: [exists x. p(x), p(a), !exists x. p(x)]
Tactics: [existsI p(a) exists x. p(x)]

Enter a step: contra p(a) !p(a)
Goal: (forall x. !p(x))
Assumptions: [!p(a), exists x. p(x), p(a), !exists x. p(x)]
Tactics: [contra p(a) !p(a)]

Enter a step: forallI a forall x. !p(x)
Goal: (forall x. !p(x))
Assumptions: [forall x. !p(x), !exists x. p(x)]
Tactics: [forallI a forall x. !p(x)]

Enter a step: done
Goal: (forall x. !p(x))
Assumptions: [!exists x. p(x)]
Tactics: [done]
```

forall x. !p(x) -> !exists x. p(x)

## forallI

Assumptions:

- p(a) -- a = Var a
- fixed a

Tactics:

- forallI a p(a)

Assumptions:

- forall x. p(x)

## forallE

Assumptions:

- forall x. p(x)

Tactics:

- forallE a forall x. p(x)

Assumptions:

- forall x. p(x)
- p(a) -- a = Var a

## existsI

Assumptions:

- p(a) -- a = Const a

Tactics:

- existsI a p(a)

Assumptions:

- p(a) -- a = Const a
- exists x. p(x)

## existsE

Assumptions:

- exists x. p(x)

Tactics:

- existsE a exists x. p(x)

Assumptions:

- exists x. p(x)
- p(a) -- a = Const a
