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
 * an argument as well. Each is saved as abstract::solve saves them.
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
            check,
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

        %! check
        %  The theory is an abstract framework: it has none of the rules, premises or axioms of the structured
        %  notations (r : B => H, f :=> a, f :-> a, the assumptions and contraries of ABA), which an abstract framework
        %  does not have: read as data, they would be dropped, or (r : [] -> a) taken for attacks.
        check :-
            theoryClause(Head, _),
            structuredNotation(Head), !,
            throw(error(domain_error(abstract_theory, rules), 'abstract: the theory has rules, premises or axioms (r : B => H, f :=> a): an abstract framework has arguments (a.) and attacks (a -> b.) alone')).
        check.

        structuredNotation(Head) :-
            callable(Head),
            functor(Head, Functor, _),
            member(Functor, [':', ':=>', ':->', '=>', assumption, contrary]), !.
        structuredNotation((A -> B)) :- (structuredTerm(A) ; structuredTerm(B)), !.
        structuredNotation((A <-> B)) :- (structuredTerm(A) ; structuredTerm(B)).

        % A label (r : B), a list of premises, or a conjunction of them: not an argument, but part of a rule
        structuredTerm(Term) :- nonvar(Term), (functor(Term, ':', 2) ; functor(Term, ',', 2) ; Term == [] ; Term = [_ | _]), !.

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
