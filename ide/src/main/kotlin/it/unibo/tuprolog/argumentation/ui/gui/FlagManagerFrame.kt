package it.unibo.tuprolog.argumentation.ui.gui

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.theory.MutableTheory
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.ui.gui.CustomTab
import it.unibo.tuprolog.ui.gui.PrologIDEModel
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
    private var prologStrictCompatibility: Boolean = false
    private var unrestrictedRebut: Boolean = true
    private var bpGraph: Boolean = false
    private var graphBuildMode: String = "base"
    private var argumentLabellingMode: String = "grounded"
    private var statementLabellingMode: String = "base"
    private var orderingPrinciple: String = "last"
    private var orderingComparator: String = "elitist"
    private var preferences: String = "standard"

    private var prefPrinciple: ChoiceBox<*>? = null
    private var prefComparator: ChoiceBox<*>? = null
    private var restrictedRebut: CheckBox? = null

    companion object {

        private var ideModel: PrologIDEModel? = null

        @JvmStatic
        fun customTab(customLibraries: List<AliasedLibrary>): CustomTab {
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
                setupChoiceBox("Preferences", listOf("none", "standard", "defeasible", "defeasibleAll"), "standard") {
                    flagManager.preferences = it
                    if (it == "defeasible") {
                        flagManager.prefPrinciple?.value = "last"
                        flagManager.prefComparator?.value = "normal"
                        flagManager.restrictedRebut?.isSelected = false
                    }
                    flagManager.prefPrinciple?.isDisable = it == "defeasible" || it == "none"
                    flagManager.prefComparator?.isDisable = it == "defeasible" || it == "none"
                    flagManager.restrictedRebut?.isDisable = it == "defeasible"
                },
                setupChoiceBox("Ordering Principle", listOf("last", "weakest")) {
                    flagManager.orderingPrinciple = it
                }.also { flagManager.prefPrinciple = it.children[1] as? ChoiceBox<*> },
                setupChoiceBox("Ordering Comparator", listOf("elitist", "democrat", "normal")) {
                    flagManager.orderingComparator = it
                }.also { flagManager.prefComparator = it.children[1] as? ChoiceBox<*> },
                setupCheckBox("Query Mode", flagManager.queryMode) { flagManager.queryMode = it },
                setupCheckBox("Auto Transposition", flagManager.autoTransposition) { flagManager.autoTransposition = it },
                setupCheckBox("Prolog Rules Compatibility", flagManager.prologStrictCompatibility) { flagManager.prologStrictCompatibility = it },
                setupCheckBox("Unrestricted Rebut", flagManager.unrestrictedRebut) { flagManager.unrestrictedRebut = it }
                    .also { flagManager.restrictedRebut = it.children[1] as? CheckBox },
                setupCheckBox("Meta Bp", flagManager.bpGraph) { flagManager.bpGraph = it }
            )
            return CustomTab(Tab("Arg Flags", ListView(items))) { model ->
                ideModel = model
                model.onReset.subscribe {
                    MutableTheory.empty().let { theory ->
                        setupSolver(theory, flagManager)
                        Library.aliased(
                            theory = theory,
                            alias = "prolog.argumentation.flags"
                        )
                    }.also {
                        model.customizeSolver { solver ->
                            (customLibraries + it).forEach { solver.loadLibrary(it) }
                            solver.setFlag(Unknown.name, Unknown.FAIL)
                        }
                    }
                }
                model.reset()
            }
        }

        @JvmStatic
        fun setupSolver(kb: Theory, target: FlagManagerFrame) {
            if (target.queryMode) kb.assertA(Struct.parse("queryMode"))
            if (target.autoTransposition) kb.assertA(Struct.parse("autoTransposition"))
            if (!target.unrestrictedRebut) kb.assertA(Struct.parse("graphExtension(rebutRestriction)"))
            if (target.preferences != "none") kb.assertA(Struct.parse("graphExtension(${target.preferences}Pref)"))
            if (target.prologStrictCompatibility) kb.assertA(Struct.parse("prologStrictCompatibility"))
            if (target.bpGraph) kb.assertA(Struct.parse("graphExtension(bp)"))
            kb.assertA(Struct.parse("graphBuildMode(${target.graphBuildMode})"))
            kb.assertA(Struct.parse("argumentLabellingMode(${target.argumentLabellingMode})"))
            kb.assertA(Struct.parse("statementLabellingMode(${target.statementLabellingMode})"))
            kb.assertA(Struct.parse("orderingPrinciple(${target.orderingPrinciple})"))
            kb.assertA(Struct.parse("orderingComparator(${target.orderingComparator})"))
        }

        @JvmStatic
        fun setupChoiceBox(label: String, values: Iterable<String>, default: String = values.first(), onChange: (String) -> Unit): HBox {
            return HBox(
                Label(label).also { it.prefWidth = 400.0 },
                ChoiceBox<String>().also {
                    it.prefWidth = 400.0
                    it.value = default
                    it.items.addAll(values)
                    it.setOnAction { _ ->
                        onChange(it.value)
                        ideModel?.reset()
                    }
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
                    it.setOnAction { _ ->
                        onChange(it.isSelected)
                        ideModel?.reset()
                    }
                }
            )
        }
    }
}
