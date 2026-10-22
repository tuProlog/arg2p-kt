package it.unibo.tuprolog.argumentation.core.libs.graph.labelling.bp

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

/**
 * The complete labelling with burdens of persuasion (module `bp_grounded_complete`, an argumentLabellingMode, ICLP):
 * the partial labelling, then the grounded labelling of its UND arguments, repeated until nothing changes.
 */
object BpCompleteLabeller : PrologModule("prolog.argumentation.graph.labelling.bpgroundedcomplete", "bp_grounded_complete") {
    override val prologRawTheory: String =
        """
        %==============================================================================
        % BP COMPLETE LABELLING [ICLP]
        %==============================================================================

        %! locality(-Locality)
        %  Query mode: not known to be directional (the burdens of proof order the arguments along the attacks), the
        %  label of an argument is taken to depend on the arguments connected to it, in either direction
        locality(connected).

        %! argumentLabelling
        %  The labelling, in the active context.
        argumentLabelling :-
            bp_grounded::reifyBurdenOfProofs,
            graphStore::arguments(Arguments),
            completeBpLabelling(go, Arguments).

        completeBpLabelling(stop, _).
        completeBpLabelling(go, Arguments) :-
            bp_grounded_partial::partialBpLabelling(Arguments), !,
            graphStore::labelledArguments(und, NewArgs),
            graphStore::unlabelArguments(und),
            grounded_old::groundedLabelling(NewArgs),
            graphStore::labelledArguments(und, LeftArguments),
            stopCondition(Stop, Arguments, LeftArguments),
            completeBpLabelling(Stop, LeftArguments).

        stopCondition(stop, Args, NewArgs) :-
            utils::sort(Args, SortedArgs),
            utils::sort(NewArgs, SortedArgs), !.
        stopCondition(go, _, _) :- graphStore::unlabelArguments(und).
        """.trimIndent()
}
