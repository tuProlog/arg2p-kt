package it.unibo.tuprolog.argumentation.core

import it.unibo.tuprolog.argumentation.core.libs.Debug
import it.unibo.tuprolog.argumentation.core.libs.EngineInterface
import it.unibo.tuprolog.argumentation.core.libs.RuleParser
import it.unibo.tuprolog.argumentation.core.libs.Utils
import it.unibo.tuprolog.argumentation.core.libs.extra.MetaInterpreter
import it.unibo.tuprolog.argumentation.core.libs.extra.ModuleCalls
import it.unibo.tuprolog.argumentation.core.libs.graph.AbstractMode
import it.unibo.tuprolog.argumentation.core.libs.graph.builder.ArgumentationGraphBuilder
import it.unibo.tuprolog.argumentation.core.libs.graph.extension.*
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.BpLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.CompleteLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.GroundedLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.StatementLabeller
import it.unibo.tuprolog.argumentation.core.libs.structured.StructuredMode
import it.unibo.tuprolog.solve.library.Libraries

val Arg2p = Libraries.of(
    MetaInterpreter,
    ModuleCalls,
    AbstractMode,
    ArgumentationGraphBuilder,
    AttackRestrictionHandler,
    BpMetaGraphHandler,
    DefeasiblePreferencesHandler,
    GenericDefeasiblePreferencesHandler,
    StrictPreferencesHandler,
    BpLabeller,
    CompleteLabeller,
    GroundedLabeller,
    StatementLabeller,
    AbstractMode,
    StructuredMode,
    EngineInterface,
    RuleParser,
    Utils,
    Debug
)
