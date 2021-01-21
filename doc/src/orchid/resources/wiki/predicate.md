---
---

## Flag
Some flags can be set in the application. as fact of the Prolog theory.

    disableBPcompletion.


to generate the BP labelling with no completeness requiremnet.

    demonstration.


to write in the output tab all the step that led to a parcticular labelling (according to definition 4.2 of Calegari and Sartor 2020).

## Engine Interface: API

The engine can be queried via two API:

    buildLabelSets.

build and prints (in the output tab) the argument and the statement labellings according to the provided theory.

    answerQuery(Goal, In, Out, Und).

allow to query the engine about a given _Goal_. The result are collected in the _In, Out, Und_ lists respectively.