package it.unibo.tuprolog.argumentation.ui.gui

import edu.uci.ics.jung.algorithms.layout.KKLayout
import edu.uci.ics.jung.algorithms.layout.Layout
import edu.uci.ics.jung.graph.Graph
import edu.uci.ics.jung.graph.SparseMultigraph
import edu.uci.ics.jung.graph.util.EdgeType
import edu.uci.ics.jung.visualization.VisualizationViewer
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse
import edu.uci.ics.jung.visualization.control.ModalGraphMouse
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import edu.uci.ics.jung.visualization.renderers.Renderer
import it.unibo.tuprolog.argumentation.core.mining.Argument
import it.unibo.tuprolog.argumentation.core.mining.Attack
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.TimeDuration
import it.unibo.tuprolog.solve.classic.classicWithDefaultBuiltins
import it.unibo.tuprolog.ui.gui.CustomTab
import javafx.embed.swing.SwingNode
import javafx.scene.control.Tab
import java.awt.Color
import java.awt.Dimension
import java.awt.event.ComponentAdapter
import java.awt.event.ComponentEvent
import javax.swing.JScrollPane
import javax.swing.JSplitPane
import javax.swing.JTabbedPane
import javax.swing.JTextArea
import javax.swing.JTextPane
import javax.swing.SwingUtilities

internal class ArgumentationGraphFrame {

    private val graphPane: JScrollPane = JScrollPane()
    private val classicTheoryPane: JScrollPane = JScrollPane()
    private val treeTheoryPane: JScrollPane = JScrollPane()
    val splitPane: JSplitPane = JSplitPane(JSplitPane.HORIZONTAL_SPLIT)

    init {
        val tabbedPane = JTabbedPane()
        tabbedPane.addTab("Classic", classicTheoryPane)
        tabbedPane.addTab("Tree", treeTheoryPane)
        splitPane.add(tabbedPane)
        splitPane.add(graphPane)
        splitPane.isOneTouchExpandable = true
        splitPane.dividerLocation = 150
    }

    fun printArgumentationInfo(arguments: List<Argument>, attacks: List<Attack>) {
        SwingUtilities.invokeLater {
            printGraph(this.graphPane, arguments, attacks)
            printTheory(this.classicTheoryPane, this.treeTheoryPane, arguments)
        }
        revalidate()
    }

    fun clear() {
        SwingUtilities.invokeLater {
            this.graphPane.viewport.removeAll()
            this.classicTheoryPane.viewport.removeAll()
            this.treeTheoryPane.viewport.removeAll()
        }
        revalidate()
    }

    fun revalidate() {
        SwingUtilities.invokeLater {
            this.splitPane.repaint()
        }
    }
    companion object {
        @JvmStatic
        fun customTab(): CustomTab {
            val frame = ArgumentationGraphFrame()
            val swingNode = SwingNode()
            frame.splitPane.addComponentListener(object : ComponentAdapter() {
                override fun componentResized(e: ComponentEvent) {
                    frame.revalidate()
                }
            })
            swingNode.content = frame.splitPane
            frame.clear()
            return CustomTab(Tab("Graph", swingNode)) { model ->
//                model.solveOptions = TimeDuration.MAX_VALUE
                model.onNewSolution.subscribe { event ->
                    if (!event.event.query.toString().startsWith("buildLabelSets")) {
                        frame.clear()
                    } else {
                        val solver = MutableSolver.classicWithDefaultBuiltins(
                            dynamicKb = event.dynamicKb,
                            staticKb = event.staticKb,
                        )
                        try {
                            val arguments = Argument.mineArguments(solver).toList()
                            val attacks = Attack.mineAttacks(solver, arguments).toList()
                            frame.printArgumentationInfo(arguments, attacks)
                        } catch (e: Exception) {
                            frame.clear()
                        }
                    }
                }
            }
        }

        @JvmStatic
        private fun buildGraph(arguments: List<Argument>, attacks: List<Attack>): Graph<String, String> {
            val graph: Graph<String, String> = SparseMultigraph()
            arguments.map(Argument::identifier)
                .forEach(graph::addVertex)
            attacks.forEach { x ->
                graph.addEdge(
                    x.attacker + x.attacked,
                    x.attacker,
                    x.attacked,
                    EdgeType.DIRECTED
                )
            }
            return graph
        }

        @JvmStatic
        private fun printGraph(graphPane: JScrollPane, arguments: List<Argument>, attacks: List<Attack>) {
            val layout: Layout<String, String> = KKLayout(buildGraph(arguments, attacks))
            layout.size = Dimension(350, 300)
            val vv: VisualizationViewer<String, String> = VisualizationViewer(layout)
            vv.preferredSize = Dimension(350, 300)
            vv.renderContext.setVertexFillPaintTransformer { i ->
                when (arguments.first { x -> x.identifier == i }.label) {
                    "in" -> Color.GREEN
                    "out" -> Color.RED
                    else -> Color.GRAY
                }
            }
            vv.renderContext.vertexLabelTransformer = ToStringLabeller()
            vv.renderer.vertexLabelRenderer.position = Renderer.VertexLabel.Position.AUTO
            val graphMouse: DefaultModalGraphMouse<String, String> = DefaultModalGraphMouse()
            graphMouse.setMode(ModalGraphMouse.Mode.PICKING)
            vv.graphMouse = graphMouse
            graphPane.viewport.view = vv
        }

        @JvmStatic
        private fun printTheory(classicTheoryPane: JScrollPane, treeTheoryPane: JScrollPane, arguments: List<Argument>) {
            val textArea = JTextArea()
            textArea.isEditable = false
            arguments.sortedBy { it.identifier.drop(1).toInt() }
                .forEach { x -> textArea.append(x.descriptor + "\n") }
            classicTheoryPane.viewport.view = textArea

            val textAreaTree = JTextPane()
            textAreaTree.isEditable = false
            textAreaTree.contentType = "text/html"
            textAreaTree.text = formatResolutionTree(arguments)
            treeTheoryPane.viewport.view = textAreaTree
        }

        @JvmStatic
        private fun formatResolutionTree(arguments: List<Argument>): String {
            fun tree(arg: Argument, arguments: List<Argument>): String =
                "<li>${arg.descriptor} <b>[${arg.label.uppercase()}]</b></li>" +
                    arg.supports.joinToString(separator = "") { sub ->
                        tree(
                            arguments.first { it.identifier == sub.identifier },
                            arguments
                        )
                    }.let { if (it.isNotEmpty()) "<ul>$it</ul>" else it }

            return "<html><ul>" + arguments
                .sortedBy { it.identifier.drop(1).toInt() }
                .joinToString(separator = "") { tree(it, arguments) } + "</ul></html>"
        }
    }
}
