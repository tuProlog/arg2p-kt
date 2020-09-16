% ----------------------------------------------------------------
% argumentationEngineInterface.pl
% PIKA-lab
% Year: 2019
% ---------------------------------------------------------------

buildLabelSets ([In, No, Und], [BPIN, BPOUT, BPUND]) :-
    convertAllRules,
    buildArgumentationGraph([Arguments, Attacks, Supports] ),
    argumentLabelling([Arguments, Attacks, Supports], [IN, OUT, UND]),
    argumentBPLabelling([IN, OUT, UND], [BPIN, BPOUT, BPUND]),
    statementLabelling([BPIN, BPOUT, BPUND], [In, No, Und]), !.
    
printLabelSets :-
    buildLabelSets([In, Out, Und], [IN, OUT, UND]),
    write('\n==================================================================================> '),write('\n'),
    write('\n==============================================> STATEMENT LABELLING '),write('\n'),
    write('\n==================================================================================> '),write('\n'),
    write('\n==============================================> IN '),write('\n'),
    writeList(In),write('\n'),
    write('==============================================> OUT '),write('\n'),
    writeList(Out),write('\n'),
    write('==============================================> UND '),write('\n'),
    writeList(Und),write('\n'),
    write('\n==================================================================================> '),write('\n'),
    write('\n==============================================> ARGUMENT LABELLING '),write('\n'),
    write('\n==================================================================================> '),write('\n'),
    write('\n==============================================> IN '),write('\n'),
    writeList(IN),write('\n'),
    write('==============================================> OUT '),write('\n'),
    writeList(OUT),write('\n'),
    write('==============================================> UND '),write('\n'),
    writeList(UND),write('\n').

answerQuery(Goal, YesResult, NoResult, UndResult) :- buildLabelSets([In, Out, Und], [IN, OUT, UND]),
													 findall(Goal, answerSingleQuery(Goal, In), YesResult),
													 findall(Goal, answerSingleQuery(Goal, Out), NoResult),
													 findall(Goal, answerSingleQuery(Goal, Und), UndResult).

answerSingleQuery(Goal, Set) :- member([Goal], Set).


%go([In, No, Und]) :-
%    time(buildArgumentationGraph([Arguments, Attacks, Supports] )), %Execute Goal just like call/1 and print time used
%    time(argumentLabelling([Arguments, Attacks, Supports], [IN, OUT, UND])),
%    time(statementLabelling([IN, OUT, UND], [In, No, Und])), !.


%buildLabelSets :-
%    buildArgumentationGraph([Arguments, Attacks, Supports] ),
%    argumentLabelling([Arguments, Attacks, Supports], [IN, OUT, UND]),
%    statementLabelling([IN, OUT, UND], _).
