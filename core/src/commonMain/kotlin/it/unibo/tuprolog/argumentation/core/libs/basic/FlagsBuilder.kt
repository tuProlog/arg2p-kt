package it.unibo.tuprolog.argumentation.core.libs.basic

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.extra.ModulesPath
import it.unibo.tuprolog.argumentation.core.libs.graph.ArgumentLabellingMode
import it.unibo.tuprolog.argumentation.core.libs.graph.GraphBuildMode
import it.unibo.tuprolog.argumentation.core.libs.graph.GraphExtension
import it.unibo.tuprolog.argumentation.core.libs.graph.StatementLabellingMode
import it.unibo.tuprolog.argumentation.core.libs.language.AutoTransposition
import it.unibo.tuprolog.argumentation.core.libs.language.PrologStrictCompatibility
import it.unibo.tuprolog.argumentation.core.libs.structured.QueryMode
import it.unibo.tuprolog.argumentation.core.libs.utils.OrderingComparator
import it.unibo.tuprolog.argumentation.core.libs.utils.OrderingPrinciple
import it.unibo.tuprolog.core.Clause
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
) {
    companion object {
        @JvmStatic
        fun setupSolver(target: FlagsBuilder) =
            MutableTheory.empty(Unificator.default).also { kb ->
                if (target.emptyBuilder) return@also
                target.graphExtensions.forEach { kb.assertA(Clause.parse("${GraphExtension.predicate()}(X) :- X = $it")) }
                if (target.queryMode) kb.assertA(Struct.parse(QueryMode.predicate()))
                if (target.autoTransposition) kb.assertA(Struct.parse(AutoTransposition.predicate()))
                if (target.prologStrictCompatibility) kb.assertA(Struct.parse(PrologStrictCompatibility.predicate()))
                kb.assertA(Struct.parse("${GraphBuildMode.predicate()}(${target.graphBuildMode})"))
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
