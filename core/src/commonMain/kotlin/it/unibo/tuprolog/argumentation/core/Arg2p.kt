package it.unibo.tuprolog.argumentation.core

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.utils.Debug
import it.unibo.tuprolog.argumentation.core.libs.core.DynamicLoader
import it.unibo.tuprolog.argumentation.core.libs.EngineInterface
import it.unibo.tuprolog.argumentation.core.libs.extra.MetaInterpreter
import it.unibo.tuprolog.argumentation.core.libs.extra.ModuleCalls
import it.unibo.tuprolog.argumentation.core.libs.graph.AbstractMode
import it.unibo.tuprolog.argumentation.core.libs.graph.builder.ArgumentationGraphBuilder
import it.unibo.tuprolog.argumentation.core.libs.graph.extension.*
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.BpLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.CompleteLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.GroundedLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.StatementLabeller
import it.unibo.tuprolog.argumentation.core.libs.language.RuleParser
import it.unibo.tuprolog.argumentation.core.libs.structured.StructuredMode
import it.unibo.tuprolog.argumentation.core.libs.utils.Utils
import it.unibo.tuprolog.solve.library.Libraries

interface Arg2pSolver {
    fun staticLibraries() : Iterable<ArgLibrary>
    fun dynamicLibraries() : Iterable<ArgLibrary>

    fun to2pLibraries() = Libraries.of(staticLibraries().map { it.content() })

    companion object {
        fun of(loader:  DynamicLoader, staticLibs: Iterable<ArgLibrary>, dynamicLibs: Iterable<ArgLibrary>) =
            object : Arg2pSolver {
                override fun staticLibraries() = staticLibs
                override fun dynamicLibraries() = dynamicLibs
            }.also { loader.solver = it }
    }
}

val Arg2p = DynamicLoader.let { loader ->
    Arg2pSolver.of(
        loader,
        listOf(EngineInterface, loader, Utils, Debug),
        listOf(
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
            RuleParser
        )
    )
}
