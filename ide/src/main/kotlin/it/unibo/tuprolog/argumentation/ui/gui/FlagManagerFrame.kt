package it.unibo.tuprolog.argumentation.ui.gui

import it.unibo.tuprolog.argumentation.core.libs.basic.FlagsBuilder
import it.unibo.tuprolog.argumentation.core.libs.graph.ArgumentLabellingMode
import it.unibo.tuprolog.argumentation.core.libs.graph.StatementLabellingMode
import it.unibo.tuprolog.argumentation.core.libs.utils.OrderingComparator
import it.unibo.tuprolog.argumentation.core.libs.utils.OrderingPrinciple
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.ui.gui.CustomTab
import it.unibo.tuprolog.ui.gui.TuPrologIDEModel
import javafx.collections.FXCollections
import javafx.collections.ObservableList
import javafx.geometry.Pos
import javafx.scene.control.CheckBox
import javafx.scene.control.ChoiceBox
import javafx.scene.control.Label
import javafx.scene.control.ListView
import javafx.scene.control.Tab
import javafx.scene.control.TextField
import javafx.scene.layout.HBox

internal class FlagManagerFrame private constructor() {
    private var queryMode: Boolean = true
    private var autoTransposition: Boolean = false
    private var prologStrictCompatibility: Boolean = false
    private var unrestrictedRebut: Boolean = true
    private var bpGraph: Boolean = false
    private var graphBuildMode: String = "standard_af"
    private var argumentLabellingMode: String = "grounded"
    private var statementLabellingMode: String = "statement"
    private var orderingPrinciple: String = "last"
    private var orderingComparator: String = "elitist"
    private var preferences: String = "standard"
    private var modulesPath: String = "none"

    private var prefPrinciple: ChoiceBox<String>? = null
    private var prefComparator: ChoiceBox<String>? = null
    private var restrictedRebut: CheckBox? = null

    companion object {
        private var ideModel: TuPrologIDEModel? = null

        @JvmStatic
        @Suppress("UNCHECKED_CAST")
        fun customTab(customLibraries: List<Library>): CustomTab {
            val flagManager = FlagManagerFrame()
            val items: ObservableList<HBox> =
                FXCollections.observableArrayList(
                    setupChoiceBox("Graph Build Mode", listOf("standard_af")) {
                        flagManager.graphBuildMode = it
                    },
                    setupChoiceBox(
                        "Argument Labelling Mode",
                        ArgumentLabellingMode.values(),
                        ArgumentLabellingMode.default(),
                    ) {
                        flagManager.argumentLabellingMode = it
                    },
                    setupChoiceBox(
                        "Statement Labelling Mode",
                        StatementLabellingMode.values(),
                        StatementLabellingMode.default(),
                    ) {
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
                    setupChoiceBox(
                        "Ordering Principle",
                        OrderingPrinciple.values(),
                        OrderingPrinciple.default(),
                    ) {
                        flagManager.orderingPrinciple = it
                    }.also { flagManager.prefPrinciple = it.children[1] as? ChoiceBox<String> },
                    setupChoiceBox(
                        "Ordering Comparator",
                        OrderingComparator.values(),
                        OrderingComparator.default(),
                    ) {
                        flagManager.orderingComparator = it
                    }.also { flagManager.prefComparator = it.children[1] as? ChoiceBox<String> },
                    setupCheckBox("Query Mode", flagManager.queryMode) { flagManager.queryMode = it },
                    setupCheckBox("Auto Transposition", flagManager.autoTransposition) { flagManager.autoTransposition = it },
//                    setupCheckBox(
//                        "Prolog Rules Compatibility",
//                        flagManager.prologStrictCompatibility,
//                    ) { flagManager.prologStrictCompatibility = it },
                    setupCheckBox("Unrestricted Rebut", flagManager.unrestrictedRebut) { flagManager.unrestrictedRebut = it }
                        .also { flagManager.restrictedRebut = it.children[1] as? CheckBox },
                    setupCheckBox("Meta Bp", flagManager.bpGraph) { flagManager.bpGraph = it },
                    setupTextBox("Modules Path", flagManager.modulesPath) { flagManager.modulesPath = it },
                )
            return CustomTab(Tab("Arg Flags", ListView(items))) { model ->
                ideModel = model
                model.onReset.subscribe {
                    model.customizeSolver { solver ->
                        solver.also {
                            (
                                customLibraries +
                                    FlagsBuilder(
                                        queryMode = flagManager.queryMode,
                                        autoTransposition = flagManager.autoTransposition,
                                        prologStrictCompatibility = flagManager.prologStrictCompatibility,
                                        graphBuildMode = flagManager.graphBuildMode,
                                        argumentLabellingMode = flagManager.argumentLabellingMode,
                                        statementLabellingMode = flagManager.statementLabellingMode,
                                        orderingPrinciple = flagManager.orderingPrinciple,
                                        orderingComparator = flagManager.orderingComparator,
                                        modulesPath = flagManager.modulesPath,
                                        graphExtensions =
                                            listOf(
                                                if (!flagManager.unrestrictedRebut) listOf("rebutRestriction") else emptyList(),
                                                if (flagManager.bpGraph) listOf("bp") else emptyList(),
                                                if (flagManager.preferences != "none") {
                                                    listOf("${flagManager.preferences}Pref")
                                                } else {
                                                    emptyList()
                                                },
                                            ).flatten(),
                                    ).create().content()
                            ).forEach { solver.loadLibrary(it) }
                            solver.setFlag(Unknown.name, Unknown.FAIL)
                            solver.solve(Struct.parse("loader_reset")).first()
                        }
                    }
                }
                model.reset()
            }
        }

        @JvmStatic
        fun setupChoiceBox(
            label: String,
            values: Iterable<String>,
            default: String = values.first(),
            onChange: (String) -> Unit,
        ): HBox =
            HBox(
                Label(label).also { it.prefWidth = 400.0 },
                ChoiceBox<String>().also {
                    it.prefWidth = 400.0
                    it.value = default
                    it.items.addAll(values)
                    it.setOnAction { _ ->
                        onChange(it.value)
                        ideModel?.reset()
                    }
                },
            ).also {
                it.prefHeight = 20.0
                it.alignment = Pos.CENTER_LEFT
            }

        @JvmStatic
        fun setupCheckBox(
            label: String,
            isSelected: Boolean,
            onChange: (Boolean) -> Unit,
        ): HBox =
            HBox(
                Label(label).also { it.prefWidth = 400.0 },
                CheckBox().also {
                    it.isSelected = isSelected
                    it.setOnAction { _ ->
                        onChange(it.isSelected)
                        ideModel?.reset()
                    }
                },
            )

        @JvmStatic
        fun setupTextBox(
            label: String,
            default: String,
            onChange: (String) -> Unit,
        ): HBox =
            HBox(
                Label(label).also { it.prefWidth = 400.0 },
                TextField().also {
                    it.text = default
                    it.prefWidth = 400.0
                    it.textProperty().addListener { _, _, newText ->
                        onChange(newText)
                        ideModel?.reset()
                    }
                },
            )
    }
}
