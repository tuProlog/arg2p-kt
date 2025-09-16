package it.unibo.tuprolog.argumentation.core

import it.unibo.tuprolog.argumentation.core.libs.ArgContext
import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgLoader
import it.unibo.tuprolog.argumentation.core.libs.basic.Cache
import it.unibo.tuprolog.argumentation.core.libs.basic.Context
import it.unibo.tuprolog.argumentation.core.libs.basic.DynamicLoader
import it.unibo.tuprolog.argumentation.core.libs.basic.EngineInterface
import it.unibo.tuprolog.argumentation.core.libs.extra.MetaInterpreter
import it.unibo.tuprolog.argumentation.core.libs.extra.ModuleCalls
import it.unibo.tuprolog.argumentation.core.libs.graph.AbstractMode
import it.unibo.tuprolog.argumentation.core.libs.graph.builder.ArgumentationGraphBuilder
import it.unibo.tuprolog.argumentation.core.libs.graph.extension.AttackRestrictionHandler
import it.unibo.tuprolog.argumentation.core.libs.graph.extension.BpMetaGraphHandler
import it.unibo.tuprolog.argumentation.core.libs.graph.extension.DefeasiblePreferencesHandler
import it.unibo.tuprolog.argumentation.core.libs.graph.extension.GenericDefeasiblePreferencesHandler
import it.unibo.tuprolog.argumentation.core.libs.graph.extension.StrictPreferencesHandler
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.BinaryStatementLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.BpCompleteLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.BpLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.BpPartialLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.CompleteLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.GroundedLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.GroundedLabellerOptimized
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.PassThroughStatementLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.StatementLabeller
import it.unibo.tuprolog.argumentation.core.libs.language.RuleParser
import it.unibo.tuprolog.argumentation.core.libs.structured.StructuredMode
import it.unibo.tuprolog.argumentation.core.libs.utils.AttackRelation
import it.unibo.tuprolog.argumentation.core.libs.utils.Debug
import it.unibo.tuprolog.argumentation.core.libs.utils.SuperiorityRelation
import it.unibo.tuprolog.argumentation.core.libs.utils.Utils
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.library.Runtime

interface Arg2pSolver {
    val loader: ArgLoader
    val context: ArgContext

    fun staticLibraries(): Iterable<ArgLibrary>

    fun dynamicLibraries(): Iterable<ArgLibrary>

    fun to2pLibraries() = Runtime.of(listOf(loader, context).plus(staticLibraries()).map { it.content() })

    fun operators() =
        listOf(loader, context)
            .plus(staticLibraries())
            .map { it.theoryOperators }
            .reduce(OperatorSet::plus)

    companion object {
        fun of(
            staticLibs: Iterable<ArgLibrary>,
            dynamicLibs: Iterable<ArgLibrary>,
        ) = object : Arg2pSolver {
            override val loader = DynamicLoader(this)
            override val context: ArgContext = Context()

            override fun staticLibraries() = staticLibs

            override fun dynamicLibraries() = dynamicLibs
        }

        fun default(
            staticLibs: Iterable<ArgLibrary> = emptyList(),
            dynamicLibs: Iterable<ArgLibrary> = emptyList(),
        ) = of(
            listOf(EngineInterface, Cache()) + staticLibs,
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
                AttackRelation,
                SuperiorityRelation,
                BpPartialLabeller,
                BpCompleteLabeller,
                PassThroughStatementLabeller,
                BinaryStatementLabeller,
                GroundedLabellerOptimized,
            ) + dynamicLibs,
        )
    }
}
