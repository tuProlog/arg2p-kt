package it.unibo.tuprolog.argumentation.actor.libraries.actorGraph

import it.unibo.tuprolog.argumentation.actor.libraries.actorGraph.prolog.THEORY
import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary

/**
 * The graph explored one argument at a time (module `actorGraph`), by the solvers of the actors, which label it
 * themselves: the rules are distributed among them, each looking for the arguments and the attackers it can build with
 * its own. The steps changing the graph do not apply: their query relations are explored all the same (see
 * queryExploration::extend/1), so that the conflicts of metaConflicts make their attackers found.
 */
internal object ActorGraph : PrologLibrary("actorGraph") {
    override val prologRawTheory: String = THEORY
}
