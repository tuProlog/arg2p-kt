printTheory(Rules) :-
    write('HERE THE THEORY:'),nl,
    findall(rule([Id, Body, Head]), member(rule([Id, Body, Head]), Rules), ListRules),
    utils::writeListNl(ListRules),
    findall(premise([A, B]), member(premise([A, B]), Rules), ListPremises),
    utils::writeListNl(ListPremises),
    findall(sup(A, B), member(sup(A, B), Rules), ListSups),
    utils::writeListNl(ListSups).


printArgumentationGraph(Arguments, Attacks, Supports) :-
	findall(
	    [IDPremises, '\n',  ' TOPRULE ',  TopRule, '\n', ' CONCLUSION ', RuleHead, '\n', ' BODY ', Body, '\n', ' INFO ', Info, '\n'],
	    member([IDPremises, TopRule, RuleHead, Body, Info], Arguments),
        ArgumentsToPrint
    ),
    findall((A1, ' SUPPORTS ', A2), member((A1, A2), Supports), SupportsToPrint),
	findall((A1, ' ', T, ' ', A2, ' ON ', A3),  member((T, A1, A2, A3), Attacks),  AttacksToPrint),
    write('HERE THE GROUNDED SEMI-ABSTRACT ARGUMENTATION GRAPH'),nl,
	utils::writeListNl(ArgumentsToPrint),
	utils::writeListNl(SupportsToPrint),
	utils::writeListNl(AttacksToPrint).


printArgumentLabelling([IN, OUT, UND]) :-
    write('HERE THE ARGUMENTS LABELLED IN: '),nl,
    utils::writeListNl(IN),
    write('HERE THE ARGUMENTS LABELLED OUT: '),nl,
    utils::writeListNl(OUT),
    write('HERE THE ARGUMENTS LABELLED UND: '),nl,
    utils::writeListNl(UND).


printStatementLabelling([In, Ni, Und]) :-
    write('HERE THE STATEMENTS LABELLED IN: '),nl,
    utils::writeListNl(In),
    write('HERE THE STATEMENTS LABELLED NI: '),nl,
    utils::writeListNl(Ni),
    write('HERE THE STATEMENTS LABELLED UND: '),nl,
    utils::writeListNl(Und).

debug.

writeDebug(List) :-
    debug,
    utils::writeList(List).
writeDebug(_) :- \+ debug.
