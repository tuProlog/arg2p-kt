%! printArgumentationGraph(+Arguments, +Attacks, +Supports)
%  The graph printed, its arguments part by part.
printArgumentationGraph(Arguments, Attacks, Supports) :-
    findall(
        [Rules, '\n',  ' TOPRULE ',  TopRule, '\n', ' CONCLUSION ', Conclusion, '\n', ' BODY ', Body, '\n', ' INFO ', Info, '\n'],
        (member(Argument, Arguments), store::newArgument(Rules, TopRule, Conclusion, Body, Info, Argument)),
        ArgumentsToPrint
    ),
    findall((A1, ' SUPPORTS ', A2), member((A1, A2), Supports), SupportsToPrint),
    findall((A1, ' ', T, ' ', A2, ' ON ', A3),  member((T, A1, A2, A3), Attacks),  AttacksToPrint),
    write('HERE THE ARGUMENTATION GRAPH'),nl,
    writeListNl(ArgumentsToPrint),
    writeListNl(SupportsToPrint),
    writeListNl(AttacksToPrint).


%! printArgumentLabelling(+Labels)
%  The arguments of [In, Out, Und] printed by label.
printArgumentLabelling([IN, OUT, UND]) :-
    write('HERE THE ARGUMENTS LABELLED IN: '),nl,
    writeListNl(IN),
    write('HERE THE ARGUMENTS LABELLED OUT: '),nl,
    writeListNl(OUT),
    write('HERE THE ARGUMENTS LABELLED UND: '),nl,
    writeListNl(UND).


%! printStatementLabelling(+Labels)
%  The statements of [In, Out, Und] printed by label.
printStatementLabelling([In, Ni, Und]) :-
    write('HERE THE STATEMENTS LABELLED IN: '),nl,
    writeListNl(In),
    write('HERE THE STATEMENTS LABELLED OUT: '),nl,
    writeListNl(Ni),
    write('HERE THE STATEMENTS LABELLED UND: '),nl,
    writeListNl(Und).

% writeListNl(+List): the items of List written each on a line
writeListNl([]).
writeListNl([X|Others]) :-
    write(X), nl,
    writeListNl(Others).
