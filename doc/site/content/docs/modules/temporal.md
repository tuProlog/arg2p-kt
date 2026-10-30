---
title: Time
weight: 45
---

## The axioms of time

With the flag `temporal`, a theory states what happens and what it starts or ends, and Arg2P concludes when each
fact holds: the event calculus of Kowalski and Sergot (1986), as defeasible rules concluding what holds for the
times of a period (see [Constraints]({{% ref "/docs/modules/constraints" %}}), on with the flag).

```prolog
temporal.

f1 :=> happens(e1, 1).        % e1 happens at 1
f2 :=> initiates(e1, f).      % e1 starts f
f3 :=> happens(e2, 5).
f4 :=> terminates(e2, f).     % e2 ends f
```

| Argument | Meaning |
| --- | --- |
| `holdsAt(f, T)` with `{T > 1}` | f holds after 1: the period after the start, while no end follows it |
| `holdsAt(f, T)` with `{T =< 5}` | f holds up to 5: the period before an end the theory states, while no start precedes it |
| `holdsAt(f, T)` with `{T > 1, T =< 5}` | f holds after 1 up to 5: a start and a later end, with no end in between |
| `holdsAt(f, 3)` | f holds at 3: an instance of one of them |

A fact holds after the event that starts it, up to and including the event that ends it. Starting a fact ends what
it conflicts with, as the [conflict model]({{% ref "/docs/api/flags" %}}) says (`initiates(e, -f)` ends `f`, and with
`conflict([dead], [alive]).` a start of `dead` ends `alive`), and a start while the fact still holds starts nothing
new: the earlier start keeps its period (as a late shot does not cause a death already caused).

Each period is one argument, whatever its length: no argument is built for each time, and no horizon is set. A time
asked for, by a query or by a rule (`r : happens(thirst, T), holdsAt(empty, T) => ...`), is answered by the instance
of a period containing it.

---

## Conflicts and interruptions

Everything is defeasible: an event, a start or an end can be attacked like any premise or rule, and the periods
follow. An end that is defeated ends nothing, and the period goes on to the next end.

A statement of the theory on a fact at a time attacks the periods containing it, and their instances for that time
alone:

```prolog
c1 :=> -holdsAt(f, 7).
```

`f` is contested at 7, and still holds at 3 and at 9: the period resumes after 7, from the same start. For `-f` to
go on after 7, state an event starting it there.

---

## Causality

The axioms work with the [causality]({{% ref "/docs/modules/causality" %}}) tests as written: an intervention
removing an event (`[-happens(e1, 1)]`) removes its start, and the periods are labelled again without it.

```prolog
temporal.
r2(X, Y, T) : causesE(X, Y), happens(X, T), prolog(T1 is T + 1) => happens(Y, T1).
r1(X, Y) : causesF(X, Y) => initiates(X, Y).
d1 :=> causesE(sh(X, Y), hit(X, Y)).
d2 :=> causesF(hit(X, Y), dead(Y)).
f_1 :=> happens(sh(bu, ge), 1).
f_2 :=> happens(sh(dl, ge), 3).
```

The shot at 1 is a NESS cause of `holdsAt(dead(ge), 5)`, the shot at 3 is not: its hit starts nothing, the victim
already dead.

---

## The rules

The axioms are rules of the theory, named `t_...`: they show in the graphs, and can be attacked or preferred as any
other rule.

| Rule | Concludes |
| --- | --- |
| `t_start(E, U, T)`, `t_stop(E, U, T)`, `t_ends(E, U, T)` | `started(U, T)`, `ended(U, T)` (an end of `U`, or a start of what conflicts with it) |
| `t_after(U, T1)` | `holdsAt(U, T)` with `{T > T1}` |
| `t_before(E, U, T2)` | `holdsAt(U, T)` with `{T =< T2}` |
| `t_period(U, T1, T2)` | `holdsAt(U, T)` with `{T > T1, T =< T2}` |
| `t_broken`, `t_closes`, `t_opens` | undercut a period with an end in between, the period after a start with an end after it, the period before an end with a start before it |
| `t_already`, `t_still` | undercut a start while the fact still holds, unless it ended in between |

A `prolog/1` check on a time needs a number: write it after the premise binding the time (`happens(thirst, T)`
above), as a period leaves its times open.
