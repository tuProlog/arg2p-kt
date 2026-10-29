package it.unibo.tuprolog.argumentation.core.modules.graph

import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.graph.TheoryClause
import it.unibo.tuprolog.argumentation.core.runtime.settings.Profiles
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The argumentation graph of an abstract theory (module `abstractGraph`, the graphBuilder of the profile abstract):
 * the arguments it states, `a.` or arg(a), and the attacks between them, `a -> b.` or att(a, b), and `a <-> b.` for a
 * mutual attack, facts or derived by Prolog clauses (`X -> Y :- beats(X, Y).`); an argument attacking or attacked is
 * an argument as well. Each is saved as abstract::solve saves them. The module abstractLanguage checks the theory.
 */
object AbstractGraphModule : PrologModule("prolog.argumentation.graph.abstract", "abstractGraph") {
    // the operators of the notation, for the clauses below
    override val theoryOperators: OperatorSet
        get() = super.theoryOperators + Profiles.ABSTRACT.operators

    // the clauses of the notation are read as data: ->/2 cannot be called
    override val primitives: Map<Signature, Primitive>
        get() = mapOf(TheoryClause.signature to TheoryClause)

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! buildArgumentationGraph
        %  The arguments and the attacks of the theory, saved in the context.
        buildArgumentationGraph :-
            arguments(Stated),
            attacks(Attacks),
            findall(A, (member(A, Stated) ; member(att(A, _), Attacks) ; member(att(_, A), Attacks)), All),
            utils::sortDistinct(All, Arguments),
            utils::forEach(A, Arguments, (
                abstract::abstractArgument(A, Argument),
                graphStore::saveArgument(graph, [A], Argument)
            )),
            utils::forEach(att(X, Y), Attacks, (
                abstract::abstractArgument(X, XX),
                abstract::abstractArgument(Y, YY),
                graphStore::saveAttack(abstract, XX, YY, YY)
            )).

        %! buildArguments
        %  As buildArgumentationGraph: an abstract theory has its attacks with its arguments.
        buildArguments :- buildArgumentationGraph.

        %! buildAdditionalArguments
        %  As buildArgumentationGraph, in query mode: exploring the query finds no argument in an abstract theory, which
        %  has no rule, so the graph is the whole one.
        buildAdditionalArguments :- buildArgumentationGraph.

        %! buildAttacks
        %  None more: the attacks are saved with the arguments.
        buildAttacks.

        % The arguments and the attacks the theory states or derives, in either notation
        arguments(Arguments) :-
            catch(findall(A, arg(A), Stated), error(existence_error(_, _), _), Stated = []),
            findall(A, (theoryClause(A, true), atom(A)), Atoms),
            utils::appendFast(Stated, Atoms, Arguments).
        attacks(Attacks) :-
            catch(findall(att(A, B), att(A, B), Stated), error(existence_error(_, _), _), Stated = []),
            findall(att(A, B), (theoryClause((A -> B), Body), call(Body)), Forward),
            findall(att(A, B), (theoryClause((X <-> Y), Body), call(Body), (A = X, B = Y ; A = Y, B = X)), Mutual),
            utils::appendLists([Stated, Forward, Mutual], Attacks).
        """.trimIndent()
}
