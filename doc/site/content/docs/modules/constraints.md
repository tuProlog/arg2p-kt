---
title: Constraints
weight: 40
---

## Statements over regions

With the graph extension `constraints`, a statement may hold for a **region** of numbers in place of a number:

| Term | Region |
| --- | --- |
| `from(T)` | the numbers after `T` |
| `until(T)` | the numbers up to `T`, `T` included |
| `between(A, B)` | the numbers after `A` up to `B`, `B` included |
| a number `N` | `N` alone |

A region stands for each of its numbers, with no argument built for each of them: `holdsAt(dead, from(2))` is one
argument, for every time after 2.

```prolog
graphExtension(standardPref).
graphExtension(constraints).

f1 :=> holdsAt(f, from(1)).
r1 : holdsAt(f, 5) => ok.
c1 :=> -holdsAt(f, 7).
```

`ok` is accepted: the argument for the region supports `holdsAt(f, 5)`. `-holdsAt(f, 7)` contests the region at 7
alone, not at 5.

---

## How it works

- **Restrictions.** An argument for a region supports a statement for a number or a region within it through its
  *restriction*: the same argument (rules, premises, preferences) concluding the narrower statement, built when a
  premise or a query needs it. Where the region came from a sub-argument (`r : holdsAt(f, T) => g(T).` concludes
  `g(from(1))`), the sub-argument is restricted as well, so a restriction rests on what holds at its numbers alone.
- **Conflicts.** Two statements conflict as they would for a number in common when their regions overlap:
  `-holdsAt(f, 7)` attacks the argument for `holdsAt(f, from(1))` and its restriction to 7, not the restrictions to
  5 or 9. In the whole graph, the conflict shows with the region restricted to it (`holdsAt(f, 7)`).
- **Where the regions are.** A statement has a region where it has a region term; a number against a number is
  compared as any term, so numbers used as data are unaffected.

A region can flow through a rule by unification (`T` bound to `from(1)` above). A `prolog/1` check on such a
variable needs a number: write the premise binding it to one first.

This is the treatment of constrained arguments of Constrained ABA (De Angelis, Fioravanti, Meo, Pettorossi, Proietti,
Toni, 2026), where an argument stands for its instances, inside the theory: a restriction is an instance, built when
needed, rather than a split of the argument by the semantics.
