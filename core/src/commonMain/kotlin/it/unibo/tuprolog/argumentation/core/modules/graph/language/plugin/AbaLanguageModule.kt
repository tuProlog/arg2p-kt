package it.unibo.tuprolog.argumentation.core.modules.graph.language.plugin

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The language of assumption-based argumentation (module `abaLanguage`, a graphLanguage of the profile aba), mapped
 * to the one of arg2p as flat ABA is an instance of ASPIC+ (Prakken 2010): each assumption (assumption(A)) an ordinary
 * premise asm(A), each contrary (contrary(A, C)) the conflict of C with A, one way (an argument for C attacks those
 * resting on A). Rules are the strict rules of arg2p.
 */
object AbaLanguageModule : PrologModule("prolog.argumentation.language.aba", "abaLanguage") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! premises(-Premises)
        %  The assumptions, as ordinary premises [asm(Assumption), Assumption].
        premises(Premises) :-
            findall([asm(A), A], user::assumption(A), Premises).

        %! conflicts(-Conflicts)
        %  The contraries, as the conflicts conflict([Contrary], [Assumption]).
        conflicts(Conflicts) :-
            findall(conflict([C], [A]), user::contrary(A, C), Conflicts).

        %! check
        %  The theory is flat ABA: no defeasible rule, no ordinary premise (its premises are its assumptions, its facts
        %  strict), and no rule concluding an assumption.
        check :-
            theory::rule(Id, _, _),
            \+ theory::checkStrict(Id), !,
            throw(error(domain_error(aba_theory, defeasible), 'aba: the theory has defeasible rules')).
        check :-
            theory::premise(Id, _),
            Id \= asm(_),
            \+ theory::checkStrict(Id), !,
            throw(error(domain_error(aba_theory, premise), 'aba: the theory has ordinary premises (f :=> a.): the premises of ABA are its assumptions (assumption/1), its facts strict (f :-> a.)')).
        check :-
            theory::rule(_, _, [A]),
            theory::premise(asm(A), [A]), !,
            throw(error(domain_error(flat_aba_theory, A), 'aba: a rule concludes an assumption (the theory is not flat)')).
        check.
        """.trimIndent()
}
