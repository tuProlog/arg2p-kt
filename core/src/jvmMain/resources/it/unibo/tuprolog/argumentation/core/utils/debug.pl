printTheory(Rules) :-
    write('HERE THE THEORY:'),write('\n'),
    findall(rule([Id, Body, Head]), member(rule([Id, Body, Head]), Rules), ListRules),
    writeList(ListRules),write('\n'),
    write(' '),write('\n'),
    findall(premise([A, B]), member(premise([A, B]), Rules), ListPremises),
    writeList(ListPremises),write('\n'),
    write(' '),write('\n'),
    findall(sup(A, B), member(sup(A, B), Rules), ListSups),
    writeList(ListSups),write('\n'),
    write(' '),write('\n').


printArgumentationGraph(Arguments, Attacks, Supports) :-
	findall(
	    [IDPremises, '\n',  ' TOPRULE ',  TopRule, '\n', ' CONCLUSION ', RuleHead, '\n', ' INFO ', Info, '\n'],
	    member([IDPremises, TopRule, RuleHead, Info], Arguments),
        ArgumentsToPrint
    ),
    findall((A1, ' SUPPORTS ', A2), member((A1, A2), Supports), SupportsToPrint),
	findall((A1, ' ', T, ' ', A2, ' ON ', A3),  member((T, A1, A2, A3), Attacks),  AttacksToPrint),
    write('HERE THE GROUNDED SEMI-ABSTRACT ARGUMENTATION GRAPH'),write('\n'),
	writeList(ArgumentsToPrint), write('\n'),
	writeList(SupportsToPrint), write('\n'),
	writeList(AttacksToPrint), write('\n').


printArgumentLabelling([IN, OUT, UND]) :-
    write('    '),write('\n'),
    write('HERE THE ARGUMENTS LABELLED IN: '),write('\n'),
    writeList(IN),write('\n'),
    write('    '),write('\n'),
    write('HERE THE ARGUMENTS LABELLED OUT: '),write('\n'),
    writeList(OUT),write('\n'),
    write('    '),write('\n'),
    write('HERE THE ARGUMENTS LABELLED UND: '),write('\n'),
    writeList(UND),write('\n').


printStatementLabelling([In, Ni, Und]) :-
    write('    '),write('\n'),
    write('HERE THE STATEMENTS LABELLED IN: '),write('\n'),
    writeList(In),write('\n'),
    write('    '),write('\n'),
    write('HERE THE STATEMENTS LABELLED NI: '),write('\n'),
    writeList(Ni),write('\n'),
    write('    '),write('\n'),
    write('HERE THE STATEMENTS LABELLED UND: '),write('\n'),
    writeList(Und),write('\n').
