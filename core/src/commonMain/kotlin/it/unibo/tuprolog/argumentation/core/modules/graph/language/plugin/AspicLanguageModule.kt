package it.unibo.tuprolog.argumentation.core.modules.graph.language.plugin

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The language of ASPIC+ (module `aspicLanguage`, a graphLanguage of the profiles aspic_plus and aspic): the one of
 * arg2p, well formed (Modgil and Prakken 2013). A contrary, a conflict declared one way only (conflict([X], [Y]) with
 * no conflict([Y], [X])), targets defeasible statements alone: neither an axiom nor the conclusion of a strict rule,
 * which no attack could reach. The contradictories (negations, conflicts declared both ways) are those transposition
 * closes the strict rules over.
 */
object AspicLanguageModule : PrologModule("prolog.argumentation.language.aspic", "aspicLanguage") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! premises(-Premises)
        %  None but those of the theory.
        premises([]).

        %! conflicts(-Conflicts)
        %  None but those of the theory.
        conflicts([]).

        %! check
        %  The theory is well formed: no contrary of an axiom or of the conclusion of a strict rule.
        check :-
            contrary(Contrary, Statement),
            strictly(Statement), !,
            throw(error(domain_error(well_formed_theory, Statement), 'aspic: a contrary (a conflict declared one way) targets an axiom or the conclusion of a strict rule')).
        check.

        % Contrary conflicts with Statement one way only, as the theory declares (see conflict::declared/2)
        contrary(Contrary, Statement) :-
            conflict::declared([Contrary], [Statement]),
            \+ conflict::declared([Statement], [Contrary]).

        % Statement is an axiom, or the conclusion of a strict rule
        strictly(Statement) :- theory::premise(Id, [Statement]), theory::checkStrict(Id).
        strictly(Statement) :- theory::rule(Id, _, [Statement]), theory::checkStrict(Id).
        """.trimIndent()
}
