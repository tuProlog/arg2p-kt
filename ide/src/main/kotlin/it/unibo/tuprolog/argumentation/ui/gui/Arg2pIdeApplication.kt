package it.unibo.tuprolog.argumentation.ui.gui

import it.unibo.tuprolog.argumentation.actor.libs.ActorSolver
import it.unibo.tuprolog.argumentation.causality.libs.CausalitySolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.solve.classic.ClassicSolverFactory
import it.unibo.tuprolog.ui.swing.app.WorkspacePersistence
import it.unibo.tuprolog.ui.swing.feature.SwingFeatureRendererRegistry
import it.unibo.tuprolog.ui.swing.launchSwingIde
import kotlinx.coroutines.runBlocking
import java.awt.AWTEvent
import java.awt.Frame
import java.awt.Toolkit
import java.awt.event.WindowEvent
import kotlin.system.exitProcess
import kotlin.time.Duration.Companion.minutes

object Arg2pIdeApplication {
    @JvmStatic
    fun main(args: Array<String>) {
        exitWhenAllFramesAreClosed()
        runBlocking {
            val arg2p = Arg2pSolver.default(staticLibs = emptyList(), dynamicLibs = listOf(ActorSolver(), CausalitySolver()))
            val libraries = Arg2pLibraries(arg2p)
            val extension = Arg2pGuiExtension(libraries)
            launchSwingIde(
                factory = ClassicSolverFactory,
                profileId = extension.profile.id,
                profileName = extension.profile.displayName,
                featureRenderers =
                    SwingFeatureRendererRegistry(
                        listOf(
                            ArgumentationGraphFrame.SwingRenderer(libraries),
                            FlagManagerFrame.SwingRenderer(libraries),
                        ),
                    ),
                extensions = listOf(extension),
                registerProfile = false,
                persistence = WorkspacePersistence("arg2p-ide"),
                defaultTimeout = 10.minutes,
            )
        }
    }

    // Some arg2p libraries (e.g. the actor-based solver) keep non-daemon threads alive after the IDE is closed
    private fun exitWhenAllFramesAreClosed() {
        Toolkit.getDefaultToolkit().addAWTEventListener(
            { event ->
                if (event.id == WindowEvent.WINDOW_CLOSED && Frame.getFrames().none { it.isDisplayable }) {
                    exitProcess(0)
                }
            },
            AWTEvent.WINDOW_EVENT_MASK,
        )
    }
}
