package it.unibo.tuprolog.argumentation.ui.gui

import it.unibo.argumentation.arg2p.Arg2p
import it.unibo.tuprolog.argumentation.ui.gui.graph.ArgumentationGraphFrame
import it.unibo.tuprolog.ui.gui.PrologIdeBuilder
import javafx.application.Application
import javafx.stage.Stage
import kotlin.system.exitProcess

class Arg2pIdeApplication : Application() {

    override fun start(stage: Stage) {
        try {
            PrologIdeBuilder(stage)
                .title("Arg-tuProlog IDE")
                .customLibraries(listOf(Arg2p))
                .customTabs(
                    listOf(
                        ArgumentationGraphFrame.customTab(),
                        FlagManagerFrame.customTab()
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
