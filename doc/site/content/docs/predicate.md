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

Alternatively, the framework can be queried with:

```prolog
answerQuery(Goal, In, Out, Und).
```

allowing to query the engine about a given _Goal_. The result are collected in the _In, Out, Und_ lists respectively.

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

The resolution process behaviour can be adjusted through the use of these flags. Some of them are required in order to successfully complete the evaluation process. 
Note that, in the standalone version of the library, there is no need to put them in the target theory, but it is possible to manage them through the _Arg Flags_ tab.
    
- `graphBuildMode(MODE)` [__REQUIRED__] to select the preferred way to build the argumentation graph starting from the statements. `MODE` can only assume the value `standard_af`.
- `statementLabellingMode(MODE)` [__REQUIRED__] to select the preferred way to execute the statement labelling. `MODE` can only assume the values:
  - `statement`
  - `statement_pass_through`
  - `statement_binary`
- `argumentLabellingMode(MODE)` [__REQUIRED__] to select the preferred way to execute the argument labelling. `MODE` can assume the values:
  - `conflictfree` — use the conflict-free semantics
  - `admissible` — use the admissible semantics
  - `stronglyadmissible` — use the strongly admissible semantics
  - `grounded` — use the grounded semantics
  - `complete` — use Dung’s complete semantics
  - `preferred` — use the preferred semantics
  - `semistable` — use the semi-stable semantics
  - `stable` — use the stable semantics
  - `ideal` — use the ideal semantics
  - `eager` — use the eager semantics
  - `naive` — use the naïve semantics
  - `stage` — use the stage semantics
  - `cf2` — use the CF2 semantics
  - `stage2` — use the stage2 semantics
  - `bp_grounded` — use the bp grounded semantics
  - `bp_grounded_partial` — use the bp partial semantics
  - `bp_grounded_complete` — use the bp complete semantics
- `graphExtension(MODE)` to select the preferred way to handle preferences. If absent, preference handling is disabled.`MODE` can assume the values: 
  - `standardPref` for ASPIC+ static preferences;
  - `defeasiblePref` for Dung's model for defeasible preferences;
  - `defeasibleAllPref` for the enhanced Dung's model enabling the use of all ASPIC+'s comparators;
- `orderingPrinciple(MODE)` [__REQUIRED__] to select the preferred way to choose the relevant parts of an argument in order to compare them. `MODE` can assume the values:
  - `last`
  - `weakest`
- `orderingComparator(MODE)` [__REQUIRED__] to select the comparison strategy to use in argument ranking. `MODE` can assume the values:
  - `democrat`
  - `elitist`
  - `normal`.
- `queryMode` to enable the structured evaluation in the `answerQuery/4` predicate;
- `autoTransposition` to enable the automatic closure under transposition of the target theory;
- `graphExtension(rebutRestriction)` (_Unrestricted Rebut_ in the IDE) to manage the _rebut restriction_ constraint.
- `graphExtension(bp)` (_Meta Bp_ in the IDE) to enable the meta evaluation for burden of persuasion. If enabled the _bp_ preferences must be included directly inside rules (for example `r : [] => bp(something).` or `r : [] => -bp(something).`). It can be used with any semantic. In the IDE the flag is called _Meta Bp_.
- `modulesPath(PATH)` to set the directory used to resolve external `.pl` modules.

For the accepted values, the defaults applied by `FlagsBuilder`, and the flags that can only be written
directly in a theory, see the [Flags Reference]({{% ref "/docs/flags" %}}).
