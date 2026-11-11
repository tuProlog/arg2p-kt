%! buildArgumentationGraph
%  The arguments and the attacks of the theory, as the abstract language reads them, saved in the context: an argument
%  attacking or attacked is an argument as well.
buildArgumentationGraph :-
    abstractLanguage::arguments(Stated),
    abstractLanguage::attacks(Attacks),
    findall(A, (member(A, Stated) ; member(att(A, _), Attacks) ; member(att(_, A), Attacks)), All),
    utils::sortDistinct(All, Arguments),
    save(Arguments, Attacks).

%! save(+Arguments, +Attacks)
%  The abstract framework of Arguments (atoms) and Attacks (att(A, B) terms) saved in the context, each argument
%  as abstractArgument/2 makes it. The goals of forEach are solved in the module utils: they name the modules of
%  their predicates.
save(Arguments, Attacks) :-
    utils::forEach(A, Arguments, (
        abstractGraph::abstractArgument(A, Argument),
        store::saveArgument(graph, [A], Argument)
    )),
    utils::forEach(att(X, Y), Attacks, (
        abstractGraph::abstractArgument(X, XX),
        abstractGraph::abstractArgument(Y, YY),
        store::saveAttack(abstract, XX, YY, YY)
    )).

%! abstractArgument(+A, -Argument)
%  The argument for the abstract argument A: the strict rule A alone, concluding A.
abstractArgument(A, Argument) :-
    store::infoParts(Info, [], [], []),
    store::newArgument([A], A, [A], [], Info, Argument).
