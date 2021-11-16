package it.unibo.tuprolog.argumentation.ui.gui

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.ui.gui.TuPrologIDEBuilder
import javafx.application.Application
import javafx.stage.Stage
import kotlin.system.exitProcess

class Arg2pIdeApplication : Application() {

    override fun start(stage: Stage) {
        try {
            val arg2p = Arg2pSolver.default()
            TuPrologIDEBuilder(stage)
                .title("Arg-tuProlog IDE")
                .customLibraries(arg2p.to2pLibraries().libraries)
                .customTabs(
                    listOf(
                        ArgumentationGraphFrame.customTab().also { it.tab.id = "arg-graph" },
                        FlagManagerFrame.customTab(arg2p.to2pLibraries().libraries.toList()).also { it.tab.id = "arg-flags" }
                    )
                )
                .show()
        } catch (e: Throwable) {
            e.printStackTrace()
            throw Error(e)
        }
    }

    override fun stop() {
        exitProcess(0)
    }

    companion object {
        @JvmStatic
        fun main(args: Array<String>) {
            launch(Arg2pIdeApplication::class.java)
        }
    }
}
