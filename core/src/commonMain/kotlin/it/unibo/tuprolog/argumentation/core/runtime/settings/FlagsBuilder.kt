package it.unibo.tuprolog.argumentation.core.runtime.settings

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.flags.ArgumentLabellingMode
import it.unibo.tuprolog.argumentation.core.flags.AutoTransposition
import it.unibo.tuprolog.argumentation.core.flags.GraphBuildMode
import it.unibo.tuprolog.argumentation.core.flags.GraphBuilder
import it.unibo.tuprolog.argumentation.core.flags.GraphExtension
import it.unibo.tuprolog.argumentation.core.flags.MetaConflicts
import it.unibo.tuprolog.argumentation.core.flags.MetaRules
import it.unibo.tuprolog.argumentation.core.flags.ModulesPath
import it.unibo.tuprolog.argumentation.core.flags.OrderingComparator
import it.unibo.tuprolog.argumentation.core.flags.OrderingPrinciple
import it.unibo.tuprolog.argumentation.core.flags.PrologStrictCompatibility
import it.unibo.tuprolog.argumentation.core.flags.QueryMode
import it.unibo.tuprolog.argumentation.core.flags.StatementLabellingMode
import it.unibo.tuprolog.argumentation.core.modules.ArgLibrary
import it.unibo.tuprolog.theory.MutableTheory
import it.unibo.tuprolog.unify.Unificator
import kotlin.jvm.JvmStatic

data class FlagsBuilder(
    var emptyBuilder: Boolean = false,
    var queryMode: Boolean = QueryMode.default(),
    var autoTransposition: Boolean = AutoTransposition.default(),
    var prologStrictCompatibility: Boolean = PrologStrictCompatibility.default(),
    var graphBuildMode: String = GraphBuildMode.default(),
    var argumentLabellingMode: String = ArgumentLabellingMode.default(),
    var statementLabellingMode: String = StatementLabellingMode.default(),
    var orderingPrinciple: String = OrderingPrinciple.default(),
    var orderingComparator: String = OrderingComparator.default(),
    var modulesPath: String = ModulesPath.default(),
    var graphExtensions: Iterable<String> = GraphExtension.default(),
    var metaConflicts: Boolean = MetaConflicts.default(),
    var metaRules: Boolean = MetaRules.default(),
    var graphBuilder: String = GraphBuilder.default(),
) {
    companion object {
        /** The facts stating the flags of [target] (see [Flags.of], which writes them) */
        @JvmStatic
        fun setupSolver(target: FlagsBuilder): MutableTheory = MutableTheory.of(Unificator.default, Flags.of(target).theory().clauses)
    }

    fun empty(emptyBuilder: Boolean) = apply { this.emptyBuilder = emptyBuilder }

    fun queryMode(queryMode: Boolean) = apply { this.queryMode = queryMode }

    fun prologStrictCompatibility(prologStrictCompatibility: Boolean) = apply { this.prologStrictCompatibility = prologStrictCompatibility }

    fun graphBuildMode(graphBuildMode: String) = apply { this.graphBuildMode = graphBuildMode }

    fun argumentLabellingMode(argumentLabellingMode: String) = apply { this.argumentLabellingMode = argumentLabellingMode }

    fun statementLabellingMode(statementLabellingMode: String) = apply { this.statementLabellingMode = statementLabellingMode }

    fun orderingPrinciple(orderingPrinciple: String) = apply { this.orderingPrinciple = orderingPrinciple }

    fun orderingComparator(orderingComparator: String) = apply { this.orderingComparator = orderingComparator }

    fun modulesPath(modulesPath: String) = apply { this.modulesPath = modulesPath }

    fun graphExtensions(graphExtensions: Iterable<String>) = apply { this.graphExtensions = graphExtensions }

    fun metaConflicts(metaConflicts: Boolean) = apply { this.metaConflicts = metaConflicts }

    fun metaRules(metaRules: Boolean) = apply { this.metaRules = metaRules }

    fun graphBuilder(graphBuilder: String) = apply { this.graphBuilder = graphBuilder }

    /** The library of the flags, for a solver of arg2p (the one of [Flags]) */
    fun create(): ArgLibrary = Flags.of(this).library()
}
