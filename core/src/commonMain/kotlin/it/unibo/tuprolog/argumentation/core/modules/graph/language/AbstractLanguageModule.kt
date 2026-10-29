package it.unibo.tuprolog.argumentation.core.modules.graph.language

import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.graph.TheoryClause
import it.unibo.tuprolog.argumentation.core.runtime.settings.Profiles
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The language of Dung frameworks (module `abstractLanguage`, the graphLanguage of the profile abstract): arguments
 * and attacks alone, which the module abstractGraph reads. It adds no premise nor conflict; it checks the theory has
 * none of the notations of the structured profiles, which an abstract framework would drop.
 */
object AbstractLanguageModule : PrologModule("prolog.argumentation.language.abstract", "abstractLanguage") {
    // the operators of the notation, for the clauses below
    override val theoryOperators: OperatorSet
        get() = super.theoryOperators + Profiles.ABSTRACT.operators

    // the clauses of the notation are read as data: ->/2 cannot be called
    override val primitives: Map<Signature, Primitive>
        get() = mapOf(TheoryClause.signature to TheoryClause)

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! premises(-Premises)
        %  None.
        premises([]).

        %! conflicts(-Conflicts)
        %  None.
        conflicts([]).

        %! check
        %  The theory is an abstract framework: it has none of the rules, premises or axioms of the structured
        %  notations (r : B => H, f :=> a, f :-> a, the assumptions and contraries of ABA), which an abstract framework
        %  does not have: read as data, they would be dropped, or (r : [] -> a) taken for attacks. Nor has it the
        %  preferences, conflicts and burdens of arg2p (sup/2, conflict/2,3, bp/N), which no argument would use.
        check :-
            theoryClause(Head, _),
            structuredNotation(Head), !,
            throw(error(domain_error(abstract_theory, rules), 'abstract: the theory has rules, premises or axioms (r : B => H, f :=> a): an abstract framework has arguments (a.) and attacks (a -> b.) alone')).
        check :-
            theoryClause(Head, _),
            structuredFact(Head), !,
            throw(error(domain_error(abstract_theory, facts), 'abstract: the theory has preferences, conflicts or burdens (sup/2, conflict/2, bp/N): an abstract framework has arguments (a.) and attacks (a -> b.) alone')).
        check.

        structuredNotation(Head) :-
            callable(Head),
            functor(Head, Functor, _),
            member(Functor, [':', ':=>', ':->', '=>', assumption, contrary]), !.
        structuredNotation((A -> B)) :- (structuredTerm(A) ; structuredTerm(B)), !.
        structuredNotation((A <-> B)) :- (structuredTerm(A) ; structuredTerm(B)).

        % A label (r : B), a list of premises, or a conjunction of them: not an argument, but part of a rule
        structuredTerm(Term) :- nonvar(Term), (functor(Term, ':', 2) ; functor(Term, ',', 2) ; Term == [] ; Term = [_ | _]), !.

        % A preference, a conflict or a burden, as arg2p reads them in every theory
        structuredFact(Head) :-
            callable(Head),
            functor(Head, Functor, Arity),
            (Functor == sup, Arity == 2 ; Functor == conflict, (Arity == 2 ; Arity == 3) ; Functor == bp, Arity > 0), !.
        """.trimIndent()
}
