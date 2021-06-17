package it.unibo.tuprolog.argumentation.ui.gui

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.core.parsing.toClause
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.ui.gui.CustomTab
import javafx.collections.FXCollections
import javafx.collections.ObservableList
import javafx.geometry.Pos
import javafx.scene.control.CheckBox
import javafx.scene.control.ChoiceBox
import javafx.scene.control.Label
import javafx.scene.control.ListView
import javafx.scene.control.Tab
import javafx.scene.layout.HBox

internal class FlagManagerFrame private constructor() {

    private var queryMode: Boolean = true
    private var autoTransposition: Boolean = false
    private var unrestrictedRebut: Boolean = true
    private var bpGraph: Boolean = false
    private var graphBuildMode: String = "base"
    private var argumentLabellingMode: String = "grounded"
    private var statementLabellingMode: String = "base"
    private var orderingPrinciple: String = "last"
    private var orderingComparator: String = "elitist"
    private var preferences: String = "standard"

    companion object {
        @JvmStatic
        fun customTab(): CustomTab {
            val flagManager = FlagManagerFrame()
            val items: ObservableList<HBox> = FXCollections.observableArrayList(
                setupChoiceBox("Graph Build Mode", listOf("base")) {
                    flagManager.graphBuildMode = it
                },
                setupChoiceBox(
                    "Argument Labelling Mode",
                    listOf(
                        "grounded",
                        "complete",
                        "bp_grounded",
                        "bp_grounded_partial",
                        "bp_grounded_complete"
                    )
                ) {
                    flagManager.argumentLabellingMode = it
                },
                setupChoiceBox("Statement Labelling Mode", listOf("base")) {
                    flagManager.statementLabellingMode = it
                },
                setupChoiceBox("Preferences", listOf("none", "standard", "defeasible", "defeasibleAll")) {
                    flagManager.preferences = it
                },
                setupChoiceBox("Ordering Principle", listOf("last", "weakest")) {
                    flagManager.orderingPrinciple = it
                },
                setupChoiceBox("Ordering Comparator", listOf("elitist", "democrat", "normal")) {
                    flagManager.orderingComparator = it
                },
                setupCheckBox("Query Mode", flagManager.queryMode) { flagManager.queryMode = it },
                setupCheckBox("Auto Transposition", flagManager.autoTransposition) { flagManager.autoTransposition = it },
                setupCheckBox("Unrestricted Rebut", flagManager.unrestrictedRebut) { flagManager.unrestrictedRebut = it },
                setupCheckBox("Meta Bp", flagManager.bpGraph) { flagManager.bpGraph = it }
            )
            return CustomTab(Tab("Arg Flags", ListView(items))) { model ->
                model.onNewQuery.subscribe {
                    cleanSolver(it.dynamicKb)
                    setupSolver(it.dynamicKb, flagManager)
                }
            }
        }

        @JvmStatic
        fun cleanSolver(kb: Theory) {
            listOf(
                Struct.parse("queryMode").toClause(),
                Struct.parse("autoTransposition").toClause(),
                Struct.parse("graphBuildMode(_)").toClause(),
                Struct.parse("argumentLabellingMode(_)").toClause(),
                Struct.parse("statementLabellingMode(_)").toClause(),
                Struct.parse("orderingPrinciple(_)").toClause(),
                Struct.parse("orderingComparator(_)").toClause(),
                Struct.parse("graphExtension(_)").toClause()
            ).forEach { kb.retractAll(it) }
        }

        @JvmStatic
        fun setupSolver(kb: Theory, target: FlagManagerFrame) {
            if (target.queryMode) kb.assertA(Struct.parse("queryMode"))
            if (target.autoTransposition) kb.assertA(Struct.parse("autoTransposition"))
            if (!target.unrestrictedRebut) kb.assertA(Struct.parse("graphExtension(rebutRestriction)"))
            if (target.preferences != "none") kb.assertA(Struct.parse("graphExtension(${target.preferences}Pref)"))
            if (target.bpGraph) kb.assertA(Struct.parse("graphExtension(bp)"))
            kb.assertA(Struct.parse("graphBuildMode(${target.graphBuildMode})"))
            kb.assertA(Struct.parse("argumentLabellingMode(${target.argumentLabellingMode})"))
            kb.assertA(Struct.parse("statementLabellingMode(${target.statementLabellingMode})"))
            kb.assertA(Struct.parse("orderingPrinciple(${target.orderingPrinciple})"))
            kb.assertA(Struct.parse("orderingComparator(${target.orderingComparator})"))
        }

        @JvmStatic
        fun setupChoiceBox(label: String, values: Iterable<String>, onChange: (String) -> Unit): HBox {
            return HBox(
                Label(label).also { it.prefWidth = 400.0 },
                ChoiceBox<String>().also {
                    it.prefWidth = 400.0
                    it.value = values.first()
                    it.items.addAll(values)
                    it.setOnAction { _ -> onChange(it.value) }
                }
            ).also {
                it.prefHeight = 20.0
                it.alignment = Pos.CENTER_LEFT
            }
        }

        @JvmStatic
        fun setupCheckBox(label: String, isSelected: Boolean, onChange: (Boolean) -> Unit): HBox {
            return HBox(
                Label(label).also { it.prefWidth = 400.0 },
                CheckBox().also {
                    it.isSelected = isSelected
                    it.setOnAction { _ -> onChange(it.isSelected) }
                }
            )
        }
    }
}
