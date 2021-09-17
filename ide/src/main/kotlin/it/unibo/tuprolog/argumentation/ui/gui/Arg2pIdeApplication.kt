package it.unibo.tuprolog.argumentation.ui.gui

import it.unibo.tuprolog.argumentation.core.Arg2p
import it.unibo.tuprolog.argumentation.core.meta.MetaInterpreterLib
import it.unibo.tuprolog.ui.gui.TuPrologIDEBuilder
import javafx.application.Application
import javafx.stage.Stage
import kotlin.system.exitProcess

class Arg2pIdeApplication : Application() {

    override fun start(stage: Stage) {
        try {
            TuPrologIDEBuilder(stage)
                .title("Arg-tuProlog IDE")
                .customLibraries(listOf(Arg2p, MetaInterpreterLib))
                .customTabs(
                    listOf(
                        ArgumentationGraphFrame.customTab(),
                        FlagManagerFrame.customTab(listOf(Arg2p, MetaInterpreterLib))
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
