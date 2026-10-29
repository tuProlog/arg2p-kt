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

```prolog
arg2p::solve.
```

builds the labelling of the whole theory without binding anything, to read it programmatically afterwards.

## Older forms

The predicates `arg2p::solve` is made of are available too, as they were before it:

```prolog
buildLabelSets(StatementIN, StatementOUT, StatementUND).   % as solve/3
buildLabelSets([StatementIN, StatementOUT, StatementUND], [ArgumentIN, ArgumentOUT, ArgumentUND]).
buildLabelSetsSilent.                                       % as solve/0
answerQuery(Goal, In, Out, Und).                            % the labels of Goal in three lists
answerQuery(Goal, Res).                                     % as solve/2
answerQuery(Goal).                                          % as solve/1
```

`buildLabelSets/2` gives the arguments labelled as well as the statements, and

```prolog
buildLabelSets.
```

builds and prints (in the output and graph tabs) both the argument and statement labellings of the theory.
`buildLabelSetsSilent` is the predicate used by the Kotlin [`evaluate`]({{% ref "/docs/api/kotlin" %}}) helper.

## Other entry points

```prolog
argTuProlog.
```

succeeds when the Arg2P library is loaded — useful to check that a solver was assembled correctly.

## Flags

The behaviour of the resolution process is controlled by flags. They can be written directly in the theory,
set from Kotlin through `FlagsBuilder`, or edited in the _Flags_ panel of the playground.

`graphBuildMode`, `argumentLabellingMode`, `statementLabellingMode`, `orderingPrinciple` and
`orderingComparator` always need a value; everything else is optional. The complete list — accepted values,
defaults, and the flags that can only be written in a theory — is on the
[Flags Reference]({{% ref "/docs/api/flags" %}}) page.
