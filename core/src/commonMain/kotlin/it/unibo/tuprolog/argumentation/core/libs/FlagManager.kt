package it.unibo.tuprolog.argumentation.core.libs

import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.theory.MutableTheory
import kotlin.jvm.JvmStatic

data class FlagsBuilder(
    var queryMode: Boolean = true,
    var autoTransposition: Boolean = false,
    var prologStrictCompatibility: Boolean = false,
    var unrestrictedRebut: Boolean = true,
    var bpGraph: Boolean = false,
    var graphBuildMode: String = "base",
    var argumentLabellingMode: String = "grounded",
    var statementLabellingMode: String = "base",
    var orderingPrinciple: String = "last",
    var orderingComparator: String = "elitist",
    var preferences: String = "standard",
    var modulesPath: String = "none"
) {

    companion object {
        @JvmStatic
        fun setupSolver(target: FlagsBuilder) =
            MutableTheory.empty().let { kb ->
                if (target.queryMode) kb.assertA(Struct.parse("queryMode"))
                if (target.autoTransposition) kb.assertA(Struct.parse("autoTransposition"))
                if (!target.unrestrictedRebut) kb.assertA(Clause.parse("graphExtension(X) :- X = rebutRestriction"))
                if (target.preferences != "none") kb.assertA(Clause.parse("graphExtension(X) :- X = ${target.preferences}Pref"))
                if (target.prologStrictCompatibility) kb.assertA(Struct.parse("prologStrictCompatibility"))
                if (target.bpGraph) kb.assertA(Clause.parse("graphExtension(X) :- X = bp"))
                kb.assertA(Struct.parse("graphBuildMode(${target.graphBuildMode})"))
                kb.assertA(Struct.parse("argumentLabellingMode(${target.argumentLabellingMode})"))
                kb.assertA(Struct.parse("statementLabellingMode(${target.statementLabellingMode})"))
                kb.assertA(Struct.parse("orderingPrinciple(${target.orderingPrinciple})"))
                kb.assertA(Struct.parse("orderingComparator(${target.orderingComparator})"))
                kb.assertA(Struct.parse("modulesPath(${target.modulesPath})"))
            }
    }

    fun queryMode(queryMode: Boolean) = apply { this.queryMode = queryMode }
    fun prologStrictCompatibility(prologStrictCompatibility: Boolean) =
        apply { this.prologStrictCompatibility = prologStrictCompatibility }

    fun unrestrictedRebut(unrestrictedRebut: Boolean) = apply { this.unrestrictedRebut = unrestrictedRebut }
    fun bpGraph(bpGraph: Boolean) = apply { this.bpGraph = bpGraph }
    fun graphBuildMode(graphBuildMode: String) = apply { this.graphBuildMode = graphBuildMode }
    fun argumentLabellingMode(argumentLabellingMode: String) =
        apply { this.argumentLabellingMode = argumentLabellingMode }

    fun statementLabellingMode(statementLabellingMode: String) =
        apply { this.statementLabellingMode = statementLabellingMode }

    fun orderingPrinciple(orderingPrinciple: String) = apply { this.orderingPrinciple = orderingPrinciple }
    fun orderingComparator(orderingComparator: String) = apply { this.orderingComparator = orderingComparator }
    fun preferences(preferences: String) = apply { this.preferences = preferences }
    fun modulesPath(modulesPath: String) = apply { this.modulesPath = modulesPath }

    fun create() = Library.aliased(
        alias = "prolog.argumentation.flags",
        theory = setupSolver(this),
    )
}
