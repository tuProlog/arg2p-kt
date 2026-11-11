---
title: Constraints
weight: 40
bookHidden: true
bookSearchExclude: true
sitemap:
  disable: true
---

> [!WARNING]
> **Experimental.** The constraints may change in a minor version of arg2p, unlike the rest of its API (see
> [stability]({{% ref "/docs/api#stability" %}})).

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
`-holdsAt(f, 7)` defeats f at 7 alone: f still holds at 5 and at 9. The [time axioms]({{% ref "/docs/modules/temporal" %}})
turn the extension on themselves.

---

## Constraints

| Constraint | Decided |
| --- | --- |
| `X > 1`, `X >= 1`, `X < 5`, `X =< 5`, `X =:= 3`, `X = 3`, between sums of numbers and variables, products and quotients by a number (`X + 2*Y =< 10`) | over the numbers, dense (`{X > 1, X < 2}` has values), with any number of variables: the simplex, on exact rationals, decides whether they hold together |
| `X = a`, `X \= a` | the term, or any other |
| any other goal (`member(X, [a, b])`, `X =\= 3`) | run as `prolog(C)` where its rule is used |

An argument keeps the constraints of its sub-arguments, and a binding (a premise, a conflict, a query) is admitted
only while they may all hold together: `{T > 1}` supports `holdsAt(f, 5)`, not `holdsAt(f, 0)`, and a rule asking
for `{T < 0}` of it makes no argument.

---

## Pieces

An open argument holds for all its values, and so does an attack on it: the argument for every time after 1 is
attacked by `-holdsAt(f, 7)`, as it states 7 as well. An attack holding for some of its values alone splits it into
**pieces**, the argument with constraints added:

- the piece **attacked**, with the attacker's constraints (`holdsAt(f, 7)`, attacked);
- the pieces **free**, with the attacker's constraints negated one at a time (`{T > 1, T < 7}` and `{T > 1, T > 7}`,
  not attacked).

| Attacker | Pieces of `holdsAt(f, T)` with `{T > 1}` |
| --- | --- |
| `-holdsAt(f, 7)` | `T = 7` (attacked), `T < 7`, `T > 7` |
| `-holdsAt(f, T)` with `{T > 5, T =< 8}` | `T > 5, T =< 8` (attacked), `T =< 5`, `T > 8` |
| `-p(a)`, on `p(X)` | `X = a` (attacked), `X \= a` |

The pieces are split again by the other attacks they meet, until none splits further, and an argument resting on an
open one is split alike (`r : holdsAt(f, T) => g(T).` gives `g(7)`, attacked, and `g(T)` before and after 7). The
argument split stays in the graph as it is, attacked and attacking, for all its values. Supports and queries read the pieces
admitting their values, so `holdsAt(f, 9)` is answered by the piece after 7 though no rule asks for 9, and the whole
graph and query mode answer alike.

An attack that cannot be bounded on the attacked argument, its region needing variables of the attacker that the
attacked argument does not have (`c : started(X), {S > X} -> -holdsAt(f, S).`, with X open), holds for all its values.
Answers show the pieces with their constraints: `holdsAt(f, T) {T > 7}`.

The extension adds arguments and attacks; it removes none.
