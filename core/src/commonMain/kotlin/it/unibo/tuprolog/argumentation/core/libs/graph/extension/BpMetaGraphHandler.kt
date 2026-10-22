package it.unibo.tuprolog.argumentation.core.libs.graph.extension

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

abstract class BpMetaGraphHandlerBase : PrologModule("prolog.argumentation.graph.meta.bp", "bp")

expect object BpMetaGraphHandler : BpMetaGraphHandlerBase {
    override val prologRawTheory: String
}
