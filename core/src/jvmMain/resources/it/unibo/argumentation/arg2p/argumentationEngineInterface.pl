% ----------------------------------------------------------------
% argumentationEngineInterface.pl
% PIKA-lab
% Year: 2019
% ---------------------------------------------------------------

argTuProlog.

buildLabelSets :-
    buildLabelSets([In, Out, Und], [IN, OUT, UND]),
    write('\n=================================================>'),nl,
    write('\nSTATEMENT LABELLING  ============================>'),nl,
    write('\n=================================================>'),nl,
    write('IN ==============================================>'),nl,
    writeList(In),
    write('OUT =============================================>'),nl,
    writeList(Out),
    write('UND =============================================>'),nl,
    writeList(Und),
    write('\n=================================================>'),nl,
    write('\nARGUMENT LABELLING ==============================>'),nl,
    write('\n=================================================>'),nl,
    write('IN ==============================================>'),nl,
    writeList(IN),
    write('OUT =============================================>'),nl,
    writeList(OUT),
    write('UND =============================================>'),nl,
    writeList(UND),
    write('=================================================>'),nl.


buildLabelSets([In, No, Und], [BPIN, BPOUT, BPUND]) :-
    convertAllRules,
    buildArgumentationGraph([Arguments, Attacks, Supports] ),
    argumentLabelling([Arguments, Attacks, Supports], [IN, OUT, UND]),
    argumentBPLabelling([IN, OUT, UND], [BPIN, BPOUT, BPUND]),
    statementLabelling([BPIN, BPOUT, BPUND], [In, No, Und]), !,
    retractall(argsLabelling(_, _, _)),
    asserta(argsLabelling(BPIN, BPOUT, BPUND)).

answerQuery(Goal, YesResult, NoResult, UndResult) :-
    buildLabelSets([In, Out, Und], [_, _, _]),
    findall(Goal, answerSingleQuery(Goal, In), YesResult),
    findall(Goal, answerSingleQuery(Goal, Out), NoResult),
    findall(Goal, answerSingleQuery(Goal, Und), UndResult).

answerSingleQuery(Goal, Set) :-
    member([Goal], Set).

%go([In, No, Und]) :-
%    time(buildArgumentationGraph([Arguments, Attacks, Supports] )), %Execute Goal just like call/1 and print time used
%    time(argumentLabelling([Arguments, Attacks, Supports], [IN, OUT, UND])),
%    time(statementLabelling([IN, OUT, UND], [In, No, Und])), !.