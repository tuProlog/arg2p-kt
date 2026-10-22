package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

object BpCompleteLabeller : PrologModule("prolog.argumentation.graph.labelling.bpgroundedcomplete", "bp_grounded_complete") {
    override val prologRawTheory: String =
        """
        %==============================================================================
        % BP COMPLETE LABELLING [ICLP]
        %==============================================================================

        % Query mode: not known to be directional (the burdens of proof order the arguments along the attacks), the
        % label of an argument is taken to depend on the arguments connected to it, in either direction
        locality(connected).

        argumentLabelling :-
            bp_grounded::reifyBurdenOfProofs,
            findall(X, context_check(argument(X)), Arguments),
            completeBpLabelling(go, Arguments).

        completeBpLabelling(stop, _).
        completeBpLabelling(go, Arguments) :-
            bp_grounded_partial::partialBpLabelling(Arguments), !,
            findall(X, (context_check(und(X)), context_retract(und(X))), NewArgs),
            grounded_old::groundedLabelling(NewArgs),
            findall(X, context_check(und(X)), LeftArguments),
            stopCondition(Stop, Arguments, LeftArguments),
            completeBpLabelling(Stop, LeftArguments).

        stopCondition(stop, Args, NewArgs) :-
            utils::sort(Args, SortedArgs),
            utils::sort(NewArgs, SortedArgs), !.
        stopCondition(go, _, _) :- context_retract(und(_)).
        """.trimIndent()
}
