package it.unibo.tuprolog.argumentation.core.libraries.evaluation.phases

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.phases.flags.ArgumentLabellingMode
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.phases.flags.GraphBuilder
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.phases.flags.GraphExtension
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.phases.flags.StatementLabellingMode
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.phases.prolog.THEORY

/**
 * What every evaluation goes through, whatever part of the graph it builds (module `phases`), as the flags select it: the
 * theory checked against its profile, the graph builder (graphBuilder, which graphBuildMode sets), the steps changing the
 * graph (the conflicts as arguments, the graph extensions; see modifyGraph/0), the semantics (argumentLabellingMode) and the
 * statement labelling (statementLabellingMode). The modules structuredWhole and structuredPartial build the graph, the module
 * abstract states it.
 */
object PhasesLibrary : PrologLibrary("phases") {
    override val baseFlags: Iterable<ArgsFlag>
        get() = listOf(GraphExtension, ArgumentLabellingMode, StatementLabellingMode, GraphBuilder)

    override val prologRawTheory: String = THEORY
}
