---
title: Engine predicates
weight: 10
aliases:
  - /docs/predicate/
---

## Engine Interface

All the predicates below belong to the `arg2p` module, and the recommended way to call them is through the
module prefix: `arg2p::solve(In, Out, Und)`, `arg2p::solve(Goal, Res)` and so on. The unqualified form works too
where `arg2p` is a static library, as in the solvers of `Arg2pSolver.default()`, but the qualified one states where
the predicate comes from and keeps working if your own theory defines a predicate with the same name.

`arg2p::solve` is the entry point of the engine. The labelling of the whole theory:

```prolog
arg2p::solve(In, Out, Und).
```

binds the statements labelled _IN, OUT and UND_, one solution for each labelling of the semantics.

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=f1%20%3A%3D%3E%20d.%0Af2%20%3A%3D%3E%20-d.%0Asup%28f1%2C%20f2%29.&query=arg2p%3A%3Asolve%28In%2C%20Out%2C%20Und%29)
— a small theory evaluated with `arg2p::solve/3`.

The labels of the statements unifying with a goal (with `queryMode`, only the part of the graph they depend on is
built):

```prolog
arg2p::solve(Goal, Res).
```

collects them as a list of `in/1`, `out/1` or `und/1` terms, and

```prolog
arg2p::solve(Goal).
```

succeeds only if _Goal_ is labelled IN: use `arg2p::solve/2` to learn the label of a goal that is not.

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=f1%20%3A%3D%3E%20d.%0Af2%20%3A%3D%3E%20-d.&query=arg2p%3A%3Asolve%28d%2C%20Res%29)

The labels of the statements unifying with a goal in three lists, as `solve/3` gives those of the whole theory:

```prolog
arg2p::solve(Goal, In, Out, Und).
```

```prolog
arg2p::solve.
```

builds the labelling of the whole theory without binding anything, to read it programmatically afterwards (the
predicate the Kotlin [`evaluate`]({{% ref "/docs/api/kotlin" %}}) helper uses).

## Earlier forms

The entry points of earlier versions are kept for the theories and programs written for them, each calling `solve`:
use `solve` in new ones.

```prolog
buildLabelSets(StatementIN, StatementOUT, StatementUND).   % solve/3
buildLabelSetsSilent.                                       % solve/0
answerQuery(Goal, In, Out, Und).                            % solve/4
answerQuery(Goal, Res).                                     % solve/2
answerQuery(Goal).                                          % solve/1
buildLabelSets([StatementIN, StatementOUT, StatementUND], [ArgumentIN, ArgumentOUT, ArgumentUND]).
buildLabelSets.
```

`buildLabelSets/2` gives the arguments labelled as well (solve/3, then `store::argumentLabelling/3`), and
`buildLabelSets/0` prints both labellings and the graph (in the output and graph tabs, see the module `graphPrinter`).

## Other entry points

```prolog
argTuProlog.
```

succeeds when the Arg2P library is loaded — useful to check that a solver was assembled correctly.

## Flags

The behaviour of the resolution process is controlled by flags. They can be written directly in the theory,
given to the solver (`Flags`), or edited in the _Flags_ panel of the playground.

Every flag has a default, and a profile (`graphBuildMode`) presets several. The complete list — accepted values,
defaults, and the flags that can only be written in a theory — is on the
[Flags Reference]({{% ref "/docs/api/flags" %}}) page.
