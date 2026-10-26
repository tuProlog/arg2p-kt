package it.unibo.tuprolog.argumentation.core.modules.graph

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The argumentation graph of an abstract theory (module `abstractGraph`, the graphBuilder of the profile abstract):
 * the arguments it states (arg/1) and the attacks between them (att/2), facts or derived by Prolog clauses; an
 * argument attacking or attacked is an argument as well. Each is saved as abstract::solve saves them.
 */
object AbstractGraphModule : PrologModule("prolog.argumentation.graph.abstract", "abstractGraph") {
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
        %  None: no argument is derived from the others.
        buildAdditionalArguments.

        %! buildAttacks
        %  None more: the attacks are saved with the arguments.
        buildAttacks.

        % The arguments and the attacks the theory states or derives, none if it does not define them
        arguments(Arguments) :-
            catch(findall(A, arg(A), Arguments), error(existence_error(_, _), _), Arguments = []).
        attacks(Attacks) :-
            catch(findall(att(A, B), att(A, B), Attacks), error(existence_error(_, _), _), Attacks = []).
        """.trimIndent()
}
