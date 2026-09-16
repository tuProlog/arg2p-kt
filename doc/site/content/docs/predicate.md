---
title: API & Flags
weight: 30 
---

## Engine Interface

All the predicates below belong to the `arg2p` module, and the recommended way to call them is through the
module prefix — `arg2p::solve(Goal, Res)`, `arg2p::buildLabelSets(...)` and so on. The unqualified form works
too, but the qualified one states where the predicate comes from and keeps working if your own theory defines
a predicate with the same name.

The engine can be queried in two ways:

```prolog
buildLabelSets.
```

build and prints (in the output and graph tabs) both the argument and statement labellings according to the provided theory.
This predicate is also available in two others versions:

```prolog
buildLabelSets(StatementIN, StatementOUT, StatementUND).
```

returning the statements labelling divided in the three sets _IN, OUT and UND_;

```prolog
buildLabelSets([StatementIN, StatementOUT, StatementUND], [ArgumentIN, ArgumentOUT, ArgumentUND]).
```

returning both the statements and arguments labelling divided in the three sets _IN, OUT and UND_;

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=f1%20%3A%3D%3E%20d.%0Af2%20%3A%3D%3E%20-d.%0Asup%28f1%2C%20f2%29.&query=arg2p%3A%3AbuildLabelSets%28SIn%2C%20SOut%2C%20SUnd%29)
— a small theory evaluated with `buildLabelSets/3`.

Alternatively, the framework can be queried with:

```prolog
answerQuery(Goal, In, Out, Und).
```

allowing to query the engine about a given _Goal_. The result are collected in the _In, Out, Und_ lists respectively.

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=f1%20%3A%3D%3E%20d.%0Af2%20%3A%3D%3E%20-d.&query=arg2p%3A%3AanswerQuery%28d%2C%20In%2C%20Out%2C%20Und%29)

Two shorter forms are available as well:

```prolog
answerQuery(Goal, Res).
```

collecting the outcome as a list of `in/1`, `out/1` or `und/1` terms, and

```prolog
answerQuery(Goal).
```

which runs the evaluation for _Goal_ without reporting its status. Note that this form succeeds even when the
goal turns out to be undecided or rejected: use `answerQuery/2` or `answerQuery/4` to learn the actual label.

## Other entry points

```prolog
buildLabelSetsSilent.
```

performs a complete evaluation without printing anything. This is the predicate used by the Kotlin
[`evaluate`]({{% ref "/docs/kotlin-api" %}}) helper, and the one to use when the results are read
programmatically rather than displayed.

```prolog
solve.            % same as buildLabelSetsSilent
solve(Goal).      % same as answerQuery(Goal)
solve(Goal, Res). % same as answerQuery(Goal, Res)
```

are shorthands for the predicates above.

```prolog
argTuProlog.
```

succeeds when the Arg2P library is loaded — useful to check that a solver was assembled correctly.

## Flags

The behaviour of the resolution process is controlled by flags. They can be written directly in the theory,
set from Kotlin through `FlagsBuilder`, or edited in the IDE's _Arg Flags_ tab.

`graphBuildMode`, `argumentLabellingMode`, `statementLabellingMode`, `orderingPrinciple` and
`orderingComparator` always need a value; everything else is optional. The complete list — accepted values,
defaults, and the flags that can only be written in a theory — is on the
[Flags Reference]({{% ref "/docs/flags" %}}) page.
