package it.unibo.tuprolog.argumentation.core.modules.graph.language

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The defeasible part of the language of ASPIC+ (module `defeasibleLanguage`, a graphLanguage of the profile
 * aspic_defeasible): defeasible rules alone, facts as rules with no premise (`r : [] => a.`). It adds no premise nor
 * conflict; it checks the theory has no strict rule, axiom nor premise.
 */
object DefeasibleLanguageModule : PrologModule("prolog.argumentation.language.defeasible", "defeasibleLanguage") {
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
        %  The theory has defeasible rules alone.
        check :-
            (parser::checkStrict(_) ; parser::premise(_, _)), !,
            throw(error(domain_error(defeasible_theory, strict), 'aspic_defeasible: the theory has strict rules, axioms or premises')).
        check.
        """.trimIndent()
}
