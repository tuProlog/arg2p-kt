package it.unibo.tuprolog.argumentation.core.modules.graph

/**
 * The argumentation graph of an abstract theory (module `abstractGraph`, the graphBuilder of the profile abstract):
 * the arguments it states, `a.` or arg(a), and the attacks between them, `a -> b.` or att(a, b), and `a <-> b.` for a
 * mutual attack, facts or derived by Prolog clauses (`X -> Y :- beats(X, Y).`); an argument attacking or attacked is
 * an argument as well. Each is saved as abstract::solve saves them. The module abstractLanguage checks the theory.
 */
internal object AbstractGraphModule : AbstractNotationModule("prolog.argumentation.graph.abstract", "abstractGraph") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! buildArgumentationGraph
        %  The arguments and the attacks of the theory, saved in the context: an argument attacking or attacked is an
        %  argument as well.
        buildArgumentationGraph :-
            arguments(Stated),
            attacks(Attacks),
            findall(A, (member(A, Stated) ; member(att(A, _), Attacks) ; member(att(_, A), Attacks)), All),
            utils::sortDistinct(All, Arguments),
            save(Arguments, Attacks).

        %! save(+Arguments, +Attacks)
        %  The abstract framework of Arguments (atoms) and Attacks (att(A, B) terms) saved in the context, each argument
        %  as abstractArgument/2 makes it. The lists are visited natively (forEach): as the goal of a findall, every step
        %  of the saving would visit them. The goals of forEach are solved in the module utils: they name the modules of
        %  their predicates.
        save(Arguments, Attacks) :-
            utils::forEach(A, Arguments, (
                abstractGraph::abstractArgument(A, Argument),
                graphStore::saveArgument(graph, [A], Argument)
            )),
            utils::forEach(att(X, Y), Attacks, (
                abstractGraph::abstractArgument(X, XX),
                abstractGraph::abstractArgument(Y, YY),
                graphStore::saveAttack(abstract, XX, YY, YY)
            )).

        %! abstractArgument(+A, -Argument)
        %  The argument for the abstract argument A: the strict rule A alone, concluding A.
        abstractArgument(A, Argument) :-
            argument::infoParts(Info, [], [], []),
            argument::new([A], A, [A], [], Info, Argument).

        % The arguments and the attacks the theory states or derives, in either notation
        arguments(Arguments) :-
            findall(A, user::arg(A), Stated),
            findall(A, (theoryClause(A, true), atom(A)), Atoms),
            utils::appendFast(Stated, Atoms, Arguments).
        attacks(Attacks) :-
            findall(att(A, B), user::att(A, B), Stated),
            findall(att(A, B), (theoryClause((A -> B), Body), user::call(Body)), Forward),
            findall(att(A, B), (theoryClause((X <-> Y), Body), user::call(Body), (A = X, B = Y ; A = Y, B = X)), Mutual),
            utils::appendLists([Stated, Forward, Mutual], Attacks).
        """.trimIndent()
}
