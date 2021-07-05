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
import javax.swing.JScrollPane
import javax.swing.JSplitPane
import javax.swing.JTabbedPane
import javax.swing.JTextArea
import javax.swing.JTextPane
import javax.swing.SwingUtilities

internal class ArgumentationGraphFrame private constructor(val argumentationGraphPane: JSplitPane) {

    private val graphPane: JScrollPane = JScrollPane()
    private val classicTheoryPane: JScrollPane = JScrollPane()
    private val treeTheoryPane: JScrollPane = JScrollPane()

    init {
        val tabbedPane = JTabbedPane()
        tabbedPane.addTab("Classic", classicTheoryPane)
        tabbedPane.addTab("Tree", treeTheoryPane)
        argumentationGraphPane.add(tabbedPane)
        argumentationGraphPane.add(graphPane)
        argumentationGraphPane.isOneTouchExpandable = true
        argumentationGraphPane.dividerLocation = 150
    }

    fun printArgumentationInfo(arguments: List<Argument>, attacks: List<Attack>) {
        printGraph(this.graphPane, arguments, attacks)
        printTheory(this.classicTheoryPane, this.treeTheoryPane, arguments)
        graphPane.revalidate()
        classicTheoryPane.revalidate()
        treeTheoryPane.revalidate()
    }

    fun clear() {
        this.graphPane.viewport.removeAll()
        this.classicTheoryPane.viewport.removeAll()
        this.treeTheoryPane.viewport.removeAll()
        this.graphPane.revalidate()
        classicTheoryPane.revalidate()
        treeTheoryPane.revalidate()
    }

    companion object {
        @JvmStatic
        fun customTab(): CustomTab {
            val swingNode = SwingNode()
            var frame: ArgumentationGraphFrame? = null
            SwingUtilities.invokeLater {
                val splitPane = JSplitPane(JSplitPane.HORIZONTAL_SPLIT)
                swingNode.content = splitPane
                frame = ArgumentationGraphFrame(splitPane)
            }
            return CustomTab(Tab("Graph", swingNode)) { model ->
                model.timeout = TimeDuration.MAX_VALUE
                model.onNewSolution.subscribe { event ->
                    if (!event.event.query.toString().startsWith("buildLabelSets")) {
                        SwingUtilities.invokeLater {
                            frame?.clear()
                        }
                    } else {
                        val solver = MutableSolver.classicWithDefaultBuiltins(
                            dynamicKb = event.dynamicKb,
                            staticKb = event.staticKb,
                        )
                        try {
                            val arguments = Argument.mineArguments(solver).toList()
                            val attacks = Attack.mineAttacks(solver, arguments).toList()

                            SwingUtilities.invokeLater {
                                frame?.printArgumentationInfo(arguments, attacks)
                            }
                        } catch (e: Exception) {
                            SwingUtilities.invokeLater {
                                frame?.clear()
                            }
                        }
                    }
                    SwingUtilities.invokeLater {
                        frame?.argumentationGraphPane?.repaint()
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
