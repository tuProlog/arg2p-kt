package it.unibo.tuprolog.argumentation.core.libs

import it.unibo.tuprolog.argumentation.core.ArgLibraries
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.theory.MutableTheory
import kotlin.jvm.JvmStatic

interface ArgsFlag<T, G> {
    fun predicate() : String
    fun default() : T
    fun values() : G
}

interface DynamicLib {
    fun mapping() : Map<String, ArgLibraries>
}

object QueryMode : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "queryMode"
    override fun default(): Boolean = true
    override fun values() {}
}

object AutoTransposition : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "autoTransposition"
    override fun default(): Boolean = false
    override fun values() {}
}

object PrologStrictCompatibility : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "prologStrictCompatibility"
    override fun default(): Boolean = true
    override fun values() {}
}

object ModulesPath : ArgsFlag<String, Unit> {
    override fun predicate(): String = "modulesPath"
    override fun default(): String = "none"
    override fun values() {}
}

object OrderingPrinciple : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "orderingPrinciple"
    override fun default(): String = "last"
    override fun values(): Iterable<String> = listOf("last", "weakest")
}

object OrderingComparator : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "orderingComparator"
    override fun default(): String = "elitist"
    override fun values(): Iterable<String> = listOf("elitist", "democrat", "normal")
}

object GraphExtension : ArgsFlag<Iterable<String>, Iterable<String>>, DynamicLib {
    override fun predicate(): String = "graphExtension"
    override fun default(): Iterable<String> = listOf("standardPref")
    override fun values(): Iterable<String> = listOf("rebutRestriction", "bp", "standardPref", "defeasiblePref", "defeasibleAllPref")
    override fun mapping(): Map<String, ArgLibraries> = mapOf(
        "rebutRestriction" to ArgLibraries.REBUT,
        "bp" to ArgLibraries.METABP,
        "standardPref" to ArgLibraries.STRICTPREF,
        "defeasiblePref" to ArgLibraries.DEFPREF,
        "defeasibleAllPref" to ArgLibraries.GDEFPREF
    )
}

object ArgumentLabellingMode : ArgsFlag<String, Iterable<String>>, DynamicLib {
    override fun predicate(): String = "argumentLabellingMode"
    override fun default(): String = "grounded"
    override fun values(): Iterable<String> = listOf(
        "grounded",
        "complete",
        "bp_grounded",
        "bp_grounded_partial",
        "bp_grounded_complete"
    )

    override fun mapping(): Map<String, ArgLibraries> = mapOf(
        "grounded" to ArgLibraries.GROUNDED,
        "complete" to ArgLibraries.COMPLETE,
        "bp_grounded" to ArgLibraries.BP,
        "bp_grounded_partial" to ArgLibraries.BP,
        "bp_grounded_complete" to ArgLibraries.BP
    )
}

object StatementLabellingMode : ArgsFlag<String, Iterable<String>>, DynamicLib {
    override fun predicate(): String = "statementLabellingMode"
    override fun default(): String = "base"
    override fun values(): Iterable<String> = listOf("base")

    override fun mapping(): Map<String, ArgLibraries> = mapOf(
        "base" to ArgLibraries.STATEMENT
    )
}

object GraphBuildMode : ArgsFlag<String, Iterable<String>>, DynamicLib {
    override fun predicate(): String = "graphBuildMode"
    override fun default(): String = "base"
    override fun values(): Iterable<String> = listOf("base")

    override fun mapping(): Map<String, ArgLibraries> = mapOf(
        "base" to ArgLibraries.GRAPH
    )
}

data class FlagsBuilder(
    var queryMode: Boolean = QueryMode.default(),
    var autoTransposition: Boolean = AutoTransposition.default(),
    var prologStrictCompatibility: Boolean = PrologStrictCompatibility.default(),
    var graphBuildMode: String = GraphBuildMode.default(),
    var argumentLabellingMode: String = ArgumentLabellingMode.default(),
    var statementLabellingMode: String = StatementLabellingMode.default(),
    var orderingPrinciple: String = OrderingPrinciple.default(),
    var orderingComparator: String = OrderingComparator.default(),
    var modulesPath: String = ModulesPath.default(),
    var graphExtensions: Iterable<String> = GraphExtension.default()
) {

    companion object {
        @JvmStatic
        fun setupSolver(target: FlagsBuilder) =
            MutableTheory.empty().also { kb ->
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

    fun queryMode(queryMode: Boolean) = apply { this.queryMode = queryMode }
    fun prologStrictCompatibility(prologStrictCompatibility: Boolean) =
        apply { this.prologStrictCompatibility = prologStrictCompatibility }
    fun graphBuildMode(graphBuildMode: String) = apply { this.graphBuildMode = graphBuildMode }
    fun argumentLabellingMode(argumentLabellingMode: String) =
        apply { this.argumentLabellingMode = argumentLabellingMode }
    fun statementLabellingMode(statementLabellingMode: String) =
        apply { this.statementLabellingMode = statementLabellingMode }

    fun orderingPrinciple(orderingPrinciple: String) = apply { this.orderingPrinciple = orderingPrinciple }
    fun orderingComparator(orderingComparator: String) = apply { this.orderingComparator = orderingComparator }
    fun modulesPath(modulesPath: String) = apply { this.modulesPath = modulesPath }
    fun graphExtensions(graphExtensions: Iterable<String>) = apply { this.graphExtensions = graphExtensions }

    fun create() = Library.aliased(
        alias = "prolog.argumentation.flags",
        theory = setupSolver(this),
    )
}
