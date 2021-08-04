---
---

## Engine Interface: API

The engine can be queried in two ways:

    buildLabelSets.

build and prints (in the output and graph tabs) both the argument and statement labellings according to the provided theory.
This predicate is also available in two others versions:

    buildLabelSets(StatementIN, StatementOUT, StatementUND).

returning the statements labelling divided in the three sets _IN, OUT and UND_;

    buildLabelSets([StatementIN, StatementOUT, StatementUND], [ArgumentIN, ArgumentOUT, ArgumentUND]).

returning both the statements and arguments labelling divided in the three sets _IN, OUT and UND_;

Alternatively, the framework can be queried with:

    answerQuery(Goal, In, Out, Und).

allowing to query the engine about a given _Goal_. The result are collected in the _In, Out, Und_ lists respectively.

## Flags

The resolution process behaviour can be adjusted through the use of these flags. Some of them are required in order to successfully complete the evaluation process. 
Note that, in the standalone version of the library, there is no need to put them in the target theory, but it is possible to manage them through the _Arg Flags_ tab.
    
- `graphBuildMode(MODE)` [__REQUIRED__] to select the preferred way to build the argumentation graph starting from the statements. `MODE` can only assume the value `base`.
- `statementLabellingMode(MODE)` [__REQUIRED__] to select the preferred way to execute the statement labelling. `MODE` can only assume the value `base`.
- `argumentLabellingMode(MODE)` [__REQUIRED__] to select the preferred way to execute the argument labelling. `MODE` can assume the values:
  - `grounded` to use the Dung's grounded semantic; 
  - `complete` to use the Dung's complete semantic;
  - `bp_grounded` to use the bp grounded semantic (Calegari and Sartor);
  - `bp_grounded_partial` to use the bp partial semantic (Calegari and Sartor); 
  - `bp_grounded_complete` to use the bp complete semantic (Calegari and Sartor);
- `graphExtension(MODE)` to select the preferred way to handle preferences. If absent, preference handling is disabled.`MODE` can assume the values: 
  - `standardPref` for ASPIC+ static preferences;
  - `defeasiblePref` for Dung's model for defeasible preferences;
  - `defeasibleAllPref` for the enhanced Dung's model enabling the use of all ASPIC+'s comparators;
- `orderingPrinciple(MODE)` [__REQUIRED__] to select the preferred way to choose the relevant parts of an argument in order to compare them. `MODE` can assume the values `last` and `weakest`.
- `orderingComparator(MODE)` [__REQUIRED__] to select the comparison strategy to use in argument ranking. `MODE` can assume the values `democrat`, `elitist` and `normal`.
- `queryMode` to enable the structured evaluation in the `answerQuery/4` predicate. It works only for `grounded` semantic;
- `autoTransposition` to enable the automatic closure under transposition of the target theory;
- `graphExtension(rebutRestriction)` (_Unrestricted Rebut_ in the IDE) to manage the _rebut restriction_ constraint.
- `graphExtension(bp)` (_Meta Bp_ in the IDE) to enable the meta evaluation for burden of persuasion. If enabled the _bp_ preferences must be included directly inside rules (for example `r : [] => bp(something).` or `r : [] => -bp(something).`). It can be used with any semantic. In the IDE the flag is called _Meta Bp_.
- `prologStrictCompatibility` (_Prolog Rules Compatibility_ in the IDE) to enable the standard _Prolog_ syntax compatibility for the argumentation language (for example `a :- b, c.` is treated as a strict rule claiming `a`, while `a := b, c.` is the defeasible counterpart). While enabled, it is still possible to use the standard argumentation syntax. 