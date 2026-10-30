---
title: Constraints
weight: 40
---

## Arguments for sets of values

An argument concludes a ground statement: a rule whose conclusion keeps a variable that its premises leave unbound
(`r : q => p(X).`) concludes nothing, on the whole graph and under `queryMode` alike, whatever value a query asks
for. With the graph extension `constraints`, such an **open** conclusion holds for every value of its variables, and
`{ }` premises constrain them:

```prolog
graphExtension(standardPref).
graphExtension(constraints).

f1 :=> s.
r0 : s, {T > 1} => holdsAt(f, T).    % f holds at every time after 1
r1 : holdsAt(f, 5) => ok.
c1 :=> -holdsAt(f, 7).
```

`holdsAt(f, T)` is one argument, for every T after 1: no argument is built for each value. `ok` is accepted, and
`-holdsAt(f, 7)` contests the argument at 7 alone, not at 5.

---

## Constraints

A premise `{ C }` is checked as `prolog(C)` once its variables are bound, and kept on the argument while they are
not. An argument keeps the constraints of its sub-arguments still open, and a binding (a premise, a conflict, a
query) is admitted only while they may all hold together.

| Constraint | Holds for |
| --- | --- |
| `X > 1`, `X >= 1`, `X < 5`, `X =< 5`, `X =:= 3`, `X =\= 3` | the numbers so compared (a dense domain: `{X > 1, X < 2}` has values) |
| `member(X, [a, b])` | the terms listed |
| `X = a`, `X \= a` | the term, or any other |
| `C1, C2` | both |

A constraint relating two unbound variables waits until one of them is bound; any other goal is checked as by
`prolog/1` once ground.

---

## Instances

The **instance** of an argument for some of its values is an argument of its own, made of the same rules and
premises, its sub-arguments instantiated alike where the values flow:

- **Supports.** A rule using an open argument for some values (`r1` above, for 5) rests on its instance, saved in the
  graph, so the attacks on those values (and on them alone) reach it. Through a rule (`r : holdsAt(f, T) => g(T).`),
  the instance of `g(T)` for 5 rests on the instance of `holdsAt(f, T)` for 5.
- **Conflicts.** Two arguments conflict when their conclusions unify and their constraints hold together:
  `-holdsAt(f, 7)` attacks the argument for every time after 1 (it states them all), and the instance for 7, which the
  graph shows. The instances for 5 or 9 are not attacked: each instance stands or falls on its own values.

The extension adds arguments and attacks; it removes nothing. With the [flag]({{% ref "/docs/api/flags" %}})
`temporal`, it is on as well.
