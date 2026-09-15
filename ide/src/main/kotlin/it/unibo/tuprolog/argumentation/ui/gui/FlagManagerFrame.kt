package it.unibo.tuprolog.argumentation.ui.gui

import it.unibo.tuprolog.argumentation.core.libs.graph.ArgumentLabellingMode
import it.unibo.tuprolog.argumentation.core.libs.graph.StatementLabellingMode
import it.unibo.tuprolog.argumentation.core.libs.utils.OrderingComparator
import it.unibo.tuprolog.argumentation.core.libs.utils.OrderingPrinciple
import it.unibo.tuprolog.ui.gui.controller.PageAction
import it.unibo.tuprolog.ui.gui.identity.PageId
import it.unibo.tuprolog.ui.gui.model.PageFeatureState
import it.unibo.tuprolog.ui.gui.model.PageState
import it.unibo.tuprolog.ui.swing.feature.SwingFeatureContext
import it.unibo.tuprolog.ui.swing.feature.SwingFeatureRenderer
import java.awt.BorderLayout
import java.awt.Dimension
import java.awt.GridLayout
import javax.swing.BoxLayout
import javax.swing.JCheckBox
import javax.swing.JComboBox
import javax.swing.JComponent
import javax.swing.JLabel
import javax.swing.JPanel
import javax.swing.JScrollPane
import javax.swing.JTextField
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener

internal class FlagManagerFrame private constructor(
    private val libraries: Arg2pLibraries,
    private val context: SwingFeatureContext,
) : JPanel(BorderLayout()) {
    private var pageId: PageId? = null
    private var updating: Boolean = false

    private val flags: Arg2pFlags
        get() = libraries.flags

    private val prefPrinciple = choiceBox(OrderingPrinciple.values(), flags.orderingPrinciple)
    private val prefComparator = choiceBox(OrderingComparator.values(), flags.orderingComparator)
    private val unrestrictedRebut = JCheckBox().also { it.isSelected = flags.unrestrictedRebut }

    init {
        val rows = JPanel()
        rows.layout = BoxLayout(rows, BoxLayout.Y_AXIS)

        rows.addRow("Graph Build Mode", choiceBox(listOf("standard_af"), flags.graphBuildMode) { copy(graphBuildMode = it) })
        rows.addRow(
            "Argument Labelling Mode",
            choiceBox(ArgumentLabellingMode.values(), flags.argumentLabellingMode) { copy(argumentLabellingMode = it) },
        )
        rows.addRow(
            "Statement Labelling Mode",
            choiceBox(StatementLabellingMode.values(), flags.statementLabellingMode) { copy(statementLabellingMode = it) },
        )
        rows.addRow(
            "Preferences",
            choiceBox(listOf("none", "standard", "defeasible", "defeasibleAll"), flags.preferences) {
                onPreferencesChanged(it)
                copy(preferences = it).let { updated ->
                    if (it == "defeasible") {
                        updated.copy(orderingPrinciple = "last", orderingComparator = "normal", unrestrictedRebut = false)
                    } else {
                        updated
                    }
                }
            },
        )
        rows.addRow("Ordering Principle", prefPrinciple.onChange { copy(orderingPrinciple = it) })
        rows.addRow("Ordering Comparator", prefComparator.onChange { copy(orderingComparator = it) })
        rows.addRow("Query Mode", checkBox(flags.queryMode) { copy(queryMode = it) })
        rows.addRow("Auto Transposition", checkBox(flags.autoTransposition) { copy(autoTransposition = it) })
        rows.addRow("Unrestricted Rebut", unrestrictedRebut.onChange { copy(unrestrictedRebut = it) })
        rows.addRow("Meta Bp", checkBox(flags.bpGraph) { copy(bpGraph = it) })
        rows.addRow("Modules Path", textBox(flags.modulesPath) { copy(modulesPath = it) })

        add(JScrollPane(rows), BorderLayout.CENTER)
        onPreferencesChanged(flags.preferences)
    }

    private fun onPreferencesChanged(preferences: String) {
        updating = true
        try {
            if (preferences == "defeasible") {
                prefPrinciple.selectedItem = "last"
                prefComparator.selectedItem = "normal"
                unrestrictedRebut.isSelected = false
            }
            prefPrinciple.isEnabled = preferences != "defeasible" && preferences != "none"
            prefComparator.isEnabled = preferences != "defeasible" && preferences != "none"
            unrestrictedRebut.isEnabled = preferences != "defeasible"
        } finally {
            updating = false
        }
    }

    private fun change(transform: Arg2pFlags.() -> Arg2pFlags) {
        if (updating) return
        libraries.flags = libraries.flags.transform()
        pageId?.let { context.dispatch(PageAction.Reset(it)) }
    }

    private fun choiceBox(
        values: Iterable<String>,
        default: String,
        onChange: (Arg2pFlags.(String) -> Arg2pFlags)? = null,
    ): JComboBox<String> =
        JComboBox(values.toList().toTypedArray()).also { box ->
            box.selectedItem = default
            onChange?.let { box.onChange(it) }
        }

    private fun JComboBox<String>.onChange(onChange: Arg2pFlags.(String) -> Arg2pFlags): JComboBox<String> =
        also { box ->
            box.addActionListener { _ ->
                (box.selectedItem as? String)?.let { value -> change { onChange(value) } }
            }
        }

    private fun checkBox(
        isSelected: Boolean,
        onChange: Arg2pFlags.(Boolean) -> Arg2pFlags,
    ): JCheckBox = JCheckBox().also { it.isSelected = isSelected }.onChange(onChange)

    private fun JCheckBox.onChange(onChange: Arg2pFlags.(Boolean) -> Arg2pFlags): JCheckBox =
        also { box ->
            box.addActionListener { _ -> change { onChange(box.isSelected) } }
        }

    private fun textBox(
        default: String,
        onChange: Arg2pFlags.(String) -> Arg2pFlags,
    ): JTextField =
        JTextField(default).also { field ->
            field.document.addDocumentListener(
                object : DocumentListener {
                    override fun insertUpdate(e: DocumentEvent) = changed()

                    override fun removeUpdate(e: DocumentEvent) = changed()

                    override fun changedUpdate(e: DocumentEvent) = changed()

                    private fun changed() = change { onChange(field.text) }
                },
            )
        }

    private fun JPanel.addRow(
        label: String,
        component: JComponent,
    ) {
        val row = JPanel(GridLayout(1, 2))
        row.add(JLabel(label))
        row.add(component)
        row.maximumSize = Dimension(Int.MAX_VALUE, row.preferredSize.height)
        add(row)
    }

    class SwingRenderer internal constructor(
        private val libraries: Arg2pLibraries,
    ) : SwingFeatureRenderer {
        override val featureId = Arg2pGuiIds.FLAGS
        override val displayName: String = "Arg Flags"

        override fun createComponent(context: SwingFeatureContext): JComponent = FlagManagerFrame(libraries, context)

        override fun render(
            component: JComponent,
            page: PageState,
            state: PageFeatureState,
        ) {
            (component as FlagManagerFrame).pageId = page.id
        }
    }
}
