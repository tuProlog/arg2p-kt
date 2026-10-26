package it.unibo.tuprolog.argumentation.core.runtime.settings

import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
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
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.library.Library
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
        @JvmStatic
        fun setupSolver(target: FlagsBuilder) =
            MutableTheory.empty(Unificator.default).also { kb ->
                if (target.emptyBuilder) return@also
                target.graphExtensions.forEach { kb.assertA(Struct.parse("${GraphExtension.predicate()}($it)")) }
                if (target.queryMode) kb.assertA(Struct.parse(QueryMode.predicate()))
                if (target.autoTransposition) kb.assertA(Struct.parse(AutoTransposition.predicate()))
                if (target.prologStrictCompatibility) kb.assertA(Struct.parse(PrologStrictCompatibility.predicate()))
                if (target.metaConflicts) kb.assertA(Struct.parse(MetaConflicts.predicate()))
                if (target.metaRules) kb.assertA(Struct.parse(MetaRules.predicate()))
                kb.assertA(Struct.parse("${GraphBuildMode.predicate()}(${target.graphBuildMode})"))
                kb.assertA(Struct.parse("${GraphBuilder.predicate()}(${target.graphBuilder})"))
                kb.assertA(Struct.parse("${ArgumentLabellingMode.predicate()}(${target.argumentLabellingMode})"))
                kb.assertA(Struct.parse("${StatementLabellingMode.predicate()}(${target.statementLabellingMode})"))
                kb.assertA(Struct.parse("${OrderingPrinciple.predicate()}(${target.orderingPrinciple})"))
                kb.assertA(Struct.parse("${OrderingComparator.predicate()}(${target.orderingComparator})"))
                kb.assertA(Struct.parse("${ModulesPath.predicate()}(${target.modulesPath})"))
            }
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

    fun create() =
        object : ArgLibrary {
            override val alias = "prolog.argumentation.flags"

            override val baseContent: Library
                get() =
                    Library.of(
                        alias = this.alias,
                        clauses = setupSolver(this@FlagsBuilder),
                    )
            override val baseFlags: Iterable<ArgsFlag<*, *>>
                get() = emptyList()
        }
}
