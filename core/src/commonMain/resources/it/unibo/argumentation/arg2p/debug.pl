% ----------------------------------------------------------------
% Debug.pl
%
% PIKA-LAB
% Year: 2019
% ---------------------------------------------------------------

enableDebug :-
    asserta(debugArg2P).

disableDebug :-
    retractall(debugArg2P).

% ========================================================================

printTheory :-
    debugArg2P,
    write('HERE THE THEORY:'),write('\n'),
    findall(rule([Id, Body, Head]), rule([Id, Body, Head]), ListRules),
    writeList(ListRules),write('\n'),
    write(' '),write('\n'),
    findall(conflict(A, B), conflict(A, B), ListConflicts),
    writeList(ListConflicts),write('\n'),
    write(' '),write('\n'),
    findall(sup(A, B), sup(A, B), ListSups),
    writeList(ListSups),write('\n'),
    write(' '),write('\n').

printTheory.


% ========================================================================

printArgumentationGraph :-
        debugArg2P,
	findall( [IDPremises, '\n',  ' TOPRULE ',  TopRule, '\n', ' CONCLUSION ', RuleHead, '\n'],
                 (   argument([IDPremises, TopRule, RuleHead]),
                     ground(argument([IDPremises, TopRule, RuleHead]))   ),
                  ArgumentsToPrint),
        findall( (A1, ' SUPPORTS ', A2), support(A1, A2), SupportsToPrint),
	findall( (A1, ' ATTACKS ', A2),  attack(A1, A2),  AttacksToPrint),


  write('HERE THE GROUNDED SEMI-ABSTRACT ARGUMENTATION GRAPH'),write('\n'),
	writeList(ArgumentsToPrint), write('\n'),write(' '),write('\n'),
	writeList(SupportsToPrint), write('\n'),write(' '),write('\n'),
	writeList(AttacksToPrint),write('\n').

printArgumentationGraph.


% ========================================================================

printArgumentLabelling(  [IN, OUT, UND] ) :-
    debugArg2P,
    write('    '),write('\n'),
    write('HERE THE ARGUMENTS LABELLED IN: '),write('\n'),
    writeList(IN),write('\n'),
    write('    '),write('\n'),
    write('HERE THE ARGUMENTS LABELLED OUT: '),write('\n'),
    writeList(OUT),write('\n'),
    write('    '),write('\n'),
    write('HERE THE ARGUMENTS LABELLED UND: '),write('\n'),
    writeList(UND),write('\n').

printArgumentLabelling( _ ).

% ========================================================================

printStatementLabelling(  [In, Ni, Und] ) :-
    debugArg2P,
    write('    '),write('\n'),
    write('HERE THE STATEMENTS LABELLED IN: '),write('\n'),
    writeList(In),write('\n'),
    write('    '),write('\n'),
    write('HERE THE STATEMENTS LABELLED NI: '),write('\n'),
    writeList(Ni),write('\n'),
    write('    '),write('\n'),
    write('HERE THE STATEMENTS LABELLED UND: '),write('\n'),
    writeList(Und),write('\n').


printStatementLabelling(  _ ).