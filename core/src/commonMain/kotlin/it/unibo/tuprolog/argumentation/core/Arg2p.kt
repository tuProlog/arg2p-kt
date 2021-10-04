package it.unibo.tuprolog.argumentation.core

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.EngineInterface
import it.unibo.tuprolog.argumentation.core.libs.core.Cache
import it.unibo.tuprolog.argumentation.core.libs.core.DynamicLoader
import it.unibo.tuprolog.argumentation.core.libs.extra.MetaInterpreter
import it.unibo.tuprolog.argumentation.core.libs.extra.ModuleCalls
import it.unibo.tuprolog.argumentation.core.libs.graph.AbstractMode
import it.unibo.tuprolog.argumentation.core.libs.graph.builder.ArgumentationGraphBuilder
import it.unibo.tuprolog.argumentation.core.libs.graph.extension.AttackRestrictionHandler
import it.unibo.tuprolog.argumentation.core.libs.graph.extension.BpMetaGraphHandler
import it.unibo.tuprolog.argumentation.core.libs.graph.extension.DefeasiblePreferencesHandler
import it.unibo.tuprolog.argumentation.core.libs.graph.extension.GenericDefeasiblePreferencesHandler
import it.unibo.tuprolog.argumentation.core.libs.graph.extension.StrictPreferencesHandler
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.BpLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.CompleteLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.GroundedLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.StatementLabeller
import it.unibo.tuprolog.argumentation.core.libs.language.RuleParser
import it.unibo.tuprolog.argumentation.core.libs.structured.StructuredMode
import it.unibo.tuprolog.argumentation.core.libs.utils.Debug
import it.unibo.tuprolog.argumentation.core.libs.utils.SuperiorityRelation
import it.unibo.tuprolog.argumentation.core.libs.utils.Utils
import it.unibo.tuprolog.solve.library.Libraries

interface Arg2pSolver {
    val loader: DynamicLoader
    fun staticLibraries(): Iterable<ArgLibrary>
    fun dynamicLibraries(): Iterable<ArgLibrary>

    fun to2pLibraries() = Libraries.of(listOf(loader).plus(staticLibraries()).map { it.content() })

    companion object {
        fun of(staticLibs: Iterable<ArgLibrary>, dynamicLibs: Iterable<ArgLibrary>) =
            object : Arg2pSolver {
                override val loader = DynamicLoader(this)
                override fun staticLibraries() = staticLibs.onEach { it.theoryOperators = loader.content().operators }
                override fun dynamicLibraries() = dynamicLibs.onEach { it.theoryOperators = loader.content().operators }
            }
    }
}

val Arg2p: Arg2pSolver = Arg2pSolver.of(
    listOf(EngineInterface, Cache()),
    listOf(
        Utils,
        Debug,
        RuleParser,
        MetaInterpreter,
        ModuleCalls,
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
        SuperiorityRelation
    )
)
