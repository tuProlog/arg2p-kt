package it.unibo.tuprolog.argumentation.core.libraries.evaluation.pipeline

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.pipeline.flags.ArgumentLabellingMode
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.pipeline.flags.GraphBuilder
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.pipeline.flags.GraphExtension
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.pipeline.flags.QueryMode
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.pipeline.flags.StatementLabellingMode
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.pipeline.prolog.THEORY

/**
 * The evaluation of a theory (module `pipeline`), as the flags select it: the theory checked against its profile, the graph
 * built by the builder (graphBuilder, which graphBuildMode sets), whole or the part the labels of the statements for a goal
 * depend on with queryMode (see evaluate/1, graph/2 and locality/1), the steps changing it (the conflicts as arguments, the graph extensions; see
 * modifyGraph/0), the semantics (argumentLabellingMode) and the statement labelling (statementLabellingMode).
 */
object PipelineLibrary : PrologLibrary("pipeline") {
    override val baseFlags: Iterable<ArgsFlag>
        get() = listOf(GraphExtension, ArgumentLabellingMode, StatementLabellingMode, GraphBuilder, QueryMode)

    override val prologRawTheory: String = THEORY
}
