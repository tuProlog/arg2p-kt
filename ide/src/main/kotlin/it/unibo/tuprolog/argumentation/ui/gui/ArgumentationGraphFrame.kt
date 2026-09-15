package it.unibo.tuprolog.argumentation.ui.gui

import edu.uci.ics.jung.algorithms.layout.FRLayout
import edu.uci.ics.jung.algorithms.layout.Layout
import edu.uci.ics.jung.graph.Graph
import edu.uci.ics.jung.graph.SparseMultigraph
import edu.uci.ics.jung.graph.util.EdgeType
import edu.uci.ics.jung.visualization.GraphZoomScrollPane
import edu.uci.ics.jung.visualization.VisualizationViewer
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse
import edu.uci.ics.jung.visualization.control.ModalGraphMouse
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import edu.uci.ics.jung.visualization.renderers.Renderer
import it.unibo.tuprolog.argumentation.core.mining.graph
import it.unibo.tuprolog.argumentation.core.model.Attack
import it.unibo.tuprolog.argumentation.core.model.LabelledArgument
import it.unibo.tuprolog.dsl.logicProgramming
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.ui.gui.identity.PageId
import it.unibo.tuprolog.ui.gui.model.PageFeatureState
import it.unibo.tuprolog.ui.gui.model.PageState
import it.unibo.tuprolog.ui.swing.feature.SwingFeatureContext
import it.unibo.tuprolog.ui.swing.feature.SwingFeatureRenderer
import java.awt.BorderLayout
import java.awt.Color
import java.awt.Dimension
import java.awt.event.ComponentAdapter
import java.awt.event.ComponentEvent
import javax.swing.BoxLayout
import javax.swing.JButton
import javax.swing.JComponent
import javax.swing.JLabel
import javax.swing.JPanel
import javax.swing.JScrollPane
import javax.swing.JSplitPane
import javax.swing.JTabbedPane
import javax.swing.JTextArea
import javax.swing.JTextPane
import javax.swing.SwingUtilities

internal class ArgumentationGraphFrame private constructor(
    private val libraries: Arg2pLibraries,
) : JPanel(BorderLayout()) {
    private val graphPane: JPanel = JPanel(BorderLayout())
    private val classicTheoryPane: JScrollPane = JScrollPane()
    private val treeTheoryPane: JScrollPane = JScrollPane()
    private val splitPane: JSplitPane = JSplitPane(JSplitPane.HORIZONTAL_SPLIT)

    private val next: JButton =
        JButton("Next").also { button ->
            button.addActionListener {
                this.selectedContext =
                    if (this.selectedContext + 1 >= this.maxContext) this.maxContext else this.selectedContext + 1
                this.refresh()
            }
        }
    private val back: JButton =
        JButton("Back").also { button ->
            button.addActionListener {
                this.selectedContext =
                    if (this.selectedContext - 1 <= this.minContext) this.minContext else this.selectedContext - 1
                this.refresh()
            }
        }
    private val context: JLabel = JLabel()

    private val minContext: Int = 0

    @Volatile
    private var maxContext: Int = 0

    @Volatile
    private var selectedContext: Int = 0

    @Volatile
    private var mutableSolver: MutableSolver? = null

    private var shown: Pair<PageId, Long>? = null

    init {

        val panel = JPanel()
        panel.layout = BoxLayout(panel, BoxLayout.Y_AXIS)

        val buttonPanel = JPanel()
        buttonPanel.layout = BoxLayout(buttonPanel, BoxLayout.X_AXIS)
        buttonPanel.add(back)
        buttonPanel.add(next)
        buttonPanel.add(context)

        val tabbedPane = JTabbedPane()
        tabbedPane.addTab("Classic", classicTheoryPane)
        tabbedPane.addTab("Tree", treeTheoryPane)

        panel.add(tabbedPane)
        panel.add(buttonPanel)

        splitPane.add(panel)
        splitPane.add(graphPane)

        splitPane.isOneTouchExpandable = true
        splitPane.dividerLocation = 150
        splitPane.addComponentListener(
            object : ComponentAdapter() {
                override fun componentResized(e: ComponentEvent) {
                    repaintGraph()
                }
            },
        )

        add(splitPane, BorderLayout.CENTER)
        refresh()
    }

    private fun show(
        page: PageId,
        state: PageFeatureState,
    ) {
        if (shown == page to state.revision) return
        shown = page to state.revision
        if (state.values.isEmpty()) {
            mutableSolver = null
            refresh()
            return
        }
        Thread {
            try {
                val solver = libraries.newSolver()
                val active =
                    logicProgramming {
                        solver
                            .solve("context_active"(X))
                            .map {
                                it.substitution[X]!!
                                    .asNumeric()!!
                                    .intValue
                                    .toInt()
                            }.first()
                    }
                this.mutableSolver = solver
                this.selectedContext = active
                this.maxContext = active
                this.refresh()
            } catch (e: Exception) {
                this.mutableSolver = null
                this.clear()
            }
        }.start()
    }

    private fun refresh() {
        SwingUtilities.invokeLater {
            back.isEnabled = this.selectedContext > this.minContext
            next.isEnabled = this.selectedContext < this.maxContext
            context.text = this.selectedContext.toString()
        }
        mutableSolver?.also { solver ->
            Thread {
                try {
                    val graph = solver.graph(this.selectedContext)
                    SwingUtilities.invokeLater {
                        this.graphPane.removeAll()
                        this.classicTheoryPane.viewport.removeAll()
                        this.treeTheoryPane.viewport.removeAll()
                        printGraph(this.graphPane, graph.labellings, graph.attacks)
                        printTheory(this.classicTheoryPane, this.treeTheoryPane, graph.labellings)
                        this.splitPane.revalidate()
                    }
                } catch (e: Exception) {
                    this.clear()
                }
            }.start()
        } ?: clear()
    }

    private fun clear() {
        SwingUtilities.invokeLater {
            this.graphPane.removeAll()
            this.classicTheoryPane.viewport.removeAll()
            this.treeTheoryPane.viewport.removeAll()
            repaintGraph()
        }
    }

    private fun repaintGraph() {
        SwingUtilities.invokeLater {
            this.splitPane.repaint()
        }
    }

    class SwingRenderer internal constructor(
        private val libraries: Arg2pLibraries,
    ) : SwingFeatureRenderer {
        override val featureId = Arg2pGuiIds.GRAPH
        override val displayName: String = "Graph"

        override fun createComponent(context: SwingFeatureContext): JComponent = ArgumentationGraphFrame(libraries)

        override fun render(
            component: JComponent,
            page: PageState,
            state: PageFeatureState,
        ) {
            (component as ArgumentationGraphFrame).show(page.id, state)
        }
    }

    companion object {
        @JvmStatic
        private fun buildGraph(
            arguments: List<LabelledArgument>,
            attacks: List<Attack>,
        ): Graph<String, String> {
            val graph: Graph<String, String> = SparseMultigraph()
            arguments
                .map { it.argument.identifier }
                .forEach(graph::addVertex)
            attacks.forEach { x ->
                graph.addEdge(
                    x.attacker.identifier + x.target.identifier,
                    x.attacker.identifier,
                    x.target.identifier,
                    EdgeType.DIRECTED,
                )
            }
            return graph
        }

        @JvmStatic
        private fun printGraph(
            graphPane: JPanel,
            arguments: List<LabelledArgument>,
            attacks: List<Attack>,
        ) {
            val layout: Layout<String, String> = FRLayout(buildGraph(arguments, attacks))
            layout.size = Dimension(350, 300)
            val vv: VisualizationViewer<String, String> = VisualizationViewer(layout)

            vv.preferredSize = Dimension(350, 300)
            vv.renderContext.setVertexFillPaintTransformer { i ->
                when (arguments.first { x -> x.argument.identifier == i }.label) {
                    "in" -> Color.GREEN
                    "out" -> Color.RED
                    "und" -> Color.GRAY
                    else -> Color.YELLOW
                }
            }
            vv.renderContext.vertexLabelTransformer = ToStringLabeller()
            vv.renderer.vertexLabelRenderer.position = Renderer.VertexLabel.Position.AUTO
            val graphMouse: DefaultModalGraphMouse<String, String> = DefaultModalGraphMouse()
            graphMouse.setMode(ModalGraphMouse.Mode.PICKING)
            vv.graphMouse = graphMouse
            vv.addKeyListener(graphMouse.modeKeyListener)
            graphPane.add(GraphZoomScrollPane(vv), BorderLayout.CENTER)
        }

        @JvmStatic
        private fun printTheory(
            classicTheoryPane: JScrollPane,
            treeTheoryPane: JScrollPane,
            arguments: List<LabelledArgument>,
        ) {
            val textArea = JTextArea()
            textArea.isEditable = false
            arguments
                .sortedBy {
                    it.argument.identifier
                        .drop(1)
                        .toInt()
                }.forEach { x -> textArea.append(x.argument.descriptor + "\n") }
            classicTheoryPane.viewport.view = textArea

            val textAreaTree = JTextPane()
            textAreaTree.isEditable = false
            textAreaTree.contentType = "text/html"
            textAreaTree.text = formatResolutionTree(arguments)
            treeTheoryPane.viewport.view = textAreaTree
        }

        @JvmStatic
        private fun formatResolutionTree(arguments: List<LabelledArgument>): String {
            fun tree(
                arg: LabelledArgument,
                arguments: List<LabelledArgument>,
            ): String =
                "<li>${arg.argument.descriptor} <b>[${arg.label.uppercase()}]</b></li>" +
                    arg.argument.supports
                        .joinToString(separator = "") { sub ->
                            tree(
                                arguments.first { it.argument.identifier == sub.identifier },
                                arguments,
                            )
                        }.let { if (it.isNotEmpty()) "<ul>$it</ul>" else it }

            return "<html><ul>" +
                arguments
                    .sortedBy {
                        it.argument.identifier
                            .drop(1)
                            .toInt()
                    }.joinToString(separator = "") { tree(it, arguments) } + "</ul></html>"
        }
    }
}
