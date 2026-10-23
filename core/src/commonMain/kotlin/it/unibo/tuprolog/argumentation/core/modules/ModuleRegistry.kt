package it.unibo.tuprolog.argumentation.core.modules

import it.unibo.tuprolog.argumentation.core.modules.data.ArgumentModule
import it.unibo.tuprolog.argumentation.core.modules.data.AttackModule
import it.unibo.tuprolog.argumentation.core.modules.data.LabelModule
import it.unibo.tuprolog.argumentation.core.modules.data.RuleModule
import it.unibo.tuprolog.argumentation.core.modules.data.StatementTermModule
import it.unibo.tuprolog.argumentation.core.modules.data.SupportModule
import it.unibo.tuprolog.argumentation.core.modules.entry.Arg2pModule
import it.unibo.tuprolog.argumentation.core.modules.extra.InterpreterModule
import it.unibo.tuprolog.argumentation.core.modules.extra.ModuleCallsModule
import it.unibo.tuprolog.argumentation.core.modules.graph.AbstractModule
import it.unibo.tuprolog.argumentation.core.modules.graph.attack.AttackCandidatesModule
import it.unibo.tuprolog.argumentation.core.modules.graph.attack.AttackRelationModule
import it.unibo.tuprolog.argumentation.core.modules.graph.builder.StandardAfModule
import it.unibo.tuprolog.argumentation.core.modules.graph.extension.BpModule
import it.unibo.tuprolog.argumentation.core.modules.graph.extension.ConflictArgumentsModule
import it.unibo.tuprolog.argumentation.core.modules.graph.extension.DefeasibleAllPrefModule
import it.unibo.tuprolog.argumentation.core.modules.graph.extension.DefeasiblePrefModule
import it.unibo.tuprolog.argumentation.core.modules.graph.extension.RebutRestrictionModule
import it.unibo.tuprolog.argumentation.core.modules.graph.extension.StandardPrefModule
import it.unibo.tuprolog.argumentation.core.modules.graph.extension.SuperiorityModule
import it.unibo.tuprolog.argumentation.core.modules.graph.store.GraphStoreModule
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.AdmissibleModule
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.Cf2Module
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.CompleteModule
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.ConflictFreeModule
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.EagerModule
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.GroundedModule
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.GroundedOldModule
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.IdealModule
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.NaiveModule
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.PreferredModule
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.SccModule
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.SemistableModule
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.StableModule
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.Stage2Module
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.StageModule
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.StronglyAdmissibleModule
import it.unibo.tuprolog.argumentation.core.modules.labelling.bp.BpGroundedCompleteModule
import it.unibo.tuprolog.argumentation.core.modules.labelling.bp.BpGroundedModule
import it.unibo.tuprolog.argumentation.core.modules.labelling.bp.BpGroundedPartialModule
import it.unibo.tuprolog.argumentation.core.modules.labelling.statement.StatementBinaryModule
import it.unibo.tuprolog.argumentation.core.modules.labelling.statement.StatementModule
import it.unibo.tuprolog.argumentation.core.modules.labelling.statement.StatementPassThroughModule
import it.unibo.tuprolog.argumentation.core.modules.language.ConflictModule
import it.unibo.tuprolog.argumentation.core.modules.language.ParserModule
import it.unibo.tuprolog.argumentation.core.modules.language.TranspositionModule
import it.unibo.tuprolog.argumentation.core.modules.query.ArgumentSearchModule
import it.unibo.tuprolog.argumentation.core.modules.query.QueryExplorationModule
import it.unibo.tuprolog.argumentation.core.modules.query.StructuredModule
import it.unibo.tuprolog.argumentation.core.modules.utils.DebugModule
import it.unibo.tuprolog.argumentation.core.modules.utils.UtilsModule

/** The modules of arg2p, by area: a solver loads each by its first call (Module::Goal) */
object ModuleRegistry {
    /** The terms arg2p works on: the entities of argumentation graphs and of the rules */
    val data: List<ArgLibrary> = listOf(ArgumentModule, AttackModule, SupportModule, LabelModule, StatementTermModule, RuleModule)

    /** The rules of a theory, as the graph builders read them */
    val language: List<ArgLibrary> = listOf(ParserModule, TranspositionModule, ConflictModule)

    /** Building the argumentation graph, and changing it with the graph extensions */
    val graph: List<ArgLibrary> =
        listOf(
            AbstractModule,
            GraphStoreModule,
            StandardAfModule,
            ConflictArgumentsModule,
            AttackRelationModule,
            AttackCandidatesModule,
            SuperiorityModule,
            RebutRestrictionModule,
            BpModule,
            DefeasiblePrefModule,
            DefeasibleAllPrefModule,
            StandardPrefModule,
        )

    /** The semantics labelling the arguments, and the statements from them */
    val labellings: List<ArgLibrary> =
        listOf(
            GroundedModule,
            GroundedOldModule,
            CompleteModule,
            PreferredModule,
            SemistableModule,
            StableModule,
            IdealModule,
            EagerModule,
            NaiveModule,
            StageModule,
            Cf2Module,
            Stage2Module,
            ConflictFreeModule,
            AdmissibleModule,
            StronglyAdmissibleModule,
            SccModule,
            BpGroundedModule,
            BpGroundedPartialModule,
            BpGroundedCompleteModule,
            StatementModule,
            StatementPassThroughModule,
            StatementBinaryModule,
        )

    /** Answering a query with the part of the graph it depends on */
    val query: List<ArgLibrary> = listOf(StructuredModule, QueryExplorationModule, ArgumentSearchModule)

    /** The entry points, and what they rely on */
    val support: List<ArgLibrary> = listOf(Arg2pModule, UtilsModule, DebugModule, InterpreterModule, ModuleCallsModule)

    /** All the modules, each with an identifier of its own: a module is loaded by its identifier */
    val all: List<ArgLibrary> by lazy {
        (support + data + language + graph + labellings + query).also { modules ->
            val repeated =
                modules
                    .filterIsInstance<Loadable>()
                    .groupBy { it.identifier() }
                    .filterValues { it.size > 1 }
                    .keys
            require(repeated.isEmpty()) { "Modules with the same identifier: $repeated" }
        }
    }
}
