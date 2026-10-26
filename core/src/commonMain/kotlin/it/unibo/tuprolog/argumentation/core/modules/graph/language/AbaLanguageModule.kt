package it.unibo.tuprolog.argumentation.core.modules.graph.language

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
            catch(findall([asm(A), A], assumption(A), Premises), error(existence_error(_, _), _), Premises = []).

        %! conflicts(-Conflicts)
        %  The contraries, as the conflicts conflict([Contrary], [Assumption]).
        conflicts(Conflicts) :-
            catch(findall(conflict([C], [A]), contrary(A, C), Conflicts), error(existence_error(_, _), _), Conflicts = []).

        %! check
        %  The theory is flat ABA: no defeasible rule, and no rule concluding an assumption.
        check :-
            parser::rule(Id, _, _),
            \+ parser::checkStrict(Id), !,
            throw(error(domain_error(aba_theory, defeasible), 'aba: the theory has defeasible rules')).
        check :-
            parser::rule(_, _, [A]),
            parser::premise(asm(A), _), !,
            throw(error(domain_error(flat_aba_theory, A), 'aba: a rule concludes an assumption (the theory is not flat)')).
        check.
        """.trimIndent()
}
