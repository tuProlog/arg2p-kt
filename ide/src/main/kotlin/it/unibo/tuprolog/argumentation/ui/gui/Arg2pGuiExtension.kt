package it.unibo.tuprolog.argumentation.ui.gui

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.libs.basic.FlagsBuilder
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.classic.ClassicSolverFactory
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.ui.gui.extension.GuiContributions
import it.unibo.tuprolog.ui.gui.extension.GuiExtension
import it.unibo.tuprolog.ui.gui.identity.ExtensionId
import it.unibo.tuprolog.ui.gui.identity.FeatureId
import it.unibo.tuprolog.ui.gui.identity.SolverProfileId
import it.unibo.tuprolog.ui.gui.model.FeatureValue
import it.unibo.tuprolog.ui.gui.presentation.FeatureDescriptor
import it.unibo.tuprolog.ui.gui.presentation.FeaturePlacement
import it.unibo.tuprolog.ui.gui.presentation.OperatorPresentation
import it.unibo.tuprolog.ui.gui.presentation.SemanticRegion
import it.unibo.tuprolog.ui.gui.prolog.SolverFactorySession
import it.unibo.tuprolog.ui.gui.prolog.solverFactoryProfile
import it.unibo.tuprolog.ui.gui.solver.SolverProfile

internal object Arg2pGuiIds {
    val EXTENSION = ExtensionId("arg2p")
    val PROFILE = SolverProfileId("arg2p")
    val GRAPH = FeatureId("arg2p.graph")
    val FLAGS = FeatureId("arg2p.flags")

    const val SOLVED_QUERY = "solvedQuery"
}

internal data class Arg2pFlags(
    val queryMode: Boolean = true,
    val autoTransposition: Boolean = false,
    val prologStrictCompatibility: Boolean = false,
    val unrestrictedRebut: Boolean = true,
    val bpGraph: Boolean = false,
    val graphBuildMode: String = "standard_af",
    val argumentLabellingMode: String = "grounded",
    val statementLabellingMode: String = "statement",
    val orderingPrinciple: String = "last",
    val orderingComparator: String = "elitist",
    val preferences: String = "standard",
    val modulesPath: String = "none",
) {
    fun toLibrary(): Library =
        FlagsBuilder(
            queryMode = queryMode,
            autoTransposition = autoTransposition,
            prologStrictCompatibility = prologStrictCompatibility,
            graphBuildMode = graphBuildMode,
            argumentLabellingMode = argumentLabellingMode,
            statementLabellingMode = statementLabellingMode,
            orderingPrinciple = orderingPrinciple,
            orderingComparator = orderingComparator,
            modulesPath = modulesPath,
            graphExtensions =
                listOf(
                    if (!unrestrictedRebut) listOf("rebutRestriction") else emptyList(),
                    if (bpGraph) listOf("bp") else emptyList(),
                    if (preferences != "none") listOf("${preferences}Pref") else emptyList(),
                ).flatten(),
        ).create().content()
}

/** The (stateful) arg2p libraries shared by every solver the IDE creates, plus the currently selected flags. */
internal class Arg2pLibraries(
    private val arg2p: Arg2pSolver,
) {
    @Volatile
    var flags: Arg2pFlags = Arg2pFlags()

    val operators: OperatorSet
        get() = arg2p.operators()

    fun current(): List<Library> = arg2p.to2pLibraries().libraries.toList() + flags.toLibrary()

    fun newSolver(): MutableSolver =
        ClassicSolverFactory.mutableSolverOf(libraries = Runtime.of(current())).also {
            it.setFlag(Unknown.name, Unknown.FAIL)
        }

    fun reset() {
        newSolver().solve(Struct.parse("loader_reset")).first()
    }
}

internal class Arg2pGuiExtension(
    libraries: Arg2pLibraries,
) : GuiExtension {
    override val id: ExtensionId = Arg2pGuiIds.EXTENSION

    val profile: SolverProfile =
        solverFactoryProfile(
            factory = ClassicSolverFactory,
            id = Arg2pGuiIds.PROFILE,
            displayName = "Arg2p",
            solutionFeatures = ::solutionFeatures,
            newSession = { factory, request, capabilities, features, runtimeLibraries ->
                libraries.reset()
                SolverFactorySession(factory, request, capabilities, features, runtimeLibraries + libraries.current())
            },
        ).let { base ->
            base.copy(
                defaultOptions = mapOf(Unknown.name to Unknown.FAIL.toString()),
                defaultOperators =
                    base.defaultOperators +
                        libraries.operators.map { OperatorPresentation(it.functor, it.priority, it.specifier.name) },
            )
        }

    override val contributions: GuiContributions =
        GuiContributions(
            solverProfiles = listOf(profile),
            features =
                listOf(
                    FeatureDescriptor(
                        id = Arg2pGuiIds.GRAPH,
                        displayName = "Graph",
                        placement = FeaturePlacement(SemanticRegion.RESULTS),
                    ),
                    FeatureDescriptor(
                        id = Arg2pGuiIds.FLAGS,
                        displayName = "Arg Flags",
                        placement = FeaturePlacement(SemanticRegion.INSPECTOR),
                    ),
                ),
        )

    private companion object {
        // Every solution bumps the graph feature revision, so that the graph tab knows it must be mined again
        fun solutionFeatures(solution: Solution): Map<FeatureId, Map<String, FeatureValue>> =
            mapOf(Arg2pGuiIds.GRAPH to mapOf(Arg2pGuiIds.SOLVED_QUERY to FeatureValue.Text(solution.query.toString())))
    }
}
