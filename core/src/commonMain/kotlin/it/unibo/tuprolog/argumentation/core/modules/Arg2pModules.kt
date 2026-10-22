package it.unibo.tuprolog.argumentation.core.modules

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.mining.graph
import it.unibo.tuprolog.argumentation.core.modules.data.ArgumentTerm
import it.unibo.tuprolog.argumentation.core.modules.data.AttackTerm
import it.unibo.tuprolog.argumentation.core.modules.data.LabelTerm
import it.unibo.tuprolog.argumentation.core.modules.data.RuleTerm
import it.unibo.tuprolog.argumentation.core.modules.data.StatementTerm
import it.unibo.tuprolog.argumentation.core.modules.data.SupportTerm
import it.unibo.tuprolog.argumentation.core.modules.entry.EngineInterface
import it.unibo.tuprolog.argumentation.core.modules.extra.MetaInterpreter
import it.unibo.tuprolog.argumentation.core.modules.extra.ModuleCalls
import it.unibo.tuprolog.argumentation.core.modules.graph.AbstractMode
import it.unibo.tuprolog.argumentation.core.modules.graph.attack.AttackCandidates
import it.unibo.tuprolog.argumentation.core.modules.graph.attack.AttackRelation
import it.unibo.tuprolog.argumentation.core.modules.graph.builder.ArgumentationGraphBuilder
import it.unibo.tuprolog.argumentation.core.modules.graph.builder.ConflictArguments
import it.unibo.tuprolog.argumentation.core.modules.graph.extension.AttackRestrictionHandler
import it.unibo.tuprolog.argumentation.core.modules.graph.extension.BpMetaGraphHandler
import it.unibo.tuprolog.argumentation.core.modules.graph.preference.DefeasiblePreferencesHandler
import it.unibo.tuprolog.argumentation.core.modules.graph.preference.GenericDefeasiblePreferencesHandler
import it.unibo.tuprolog.argumentation.core.modules.graph.preference.StrictPreferencesHandler
import it.unibo.tuprolog.argumentation.core.modules.graph.preference.SuperiorityRelation
import it.unibo.tuprolog.argumentation.core.modules.graph.store.GraphStore
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.AdmissibleLabeller
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.Cf2Labeller
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.CompleteLabeller
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.ConflictFreeLabeller
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.EagerLabeller
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.GroundedLabeller
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.GroundedLabellerOptimized
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.IdealLabeller
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.NaiveLabeller
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.PreferredLabeller
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.SccLabelling
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.SemistableLabeller
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.StableLabeller
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.Stage2Labeller
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.StageLabeller
import it.unibo.tuprolog.argumentation.core.modules.labelling.argument.StronglyAdmissibleLabeller
import it.unibo.tuprolog.argumentation.core.modules.labelling.bp.BpCompleteLabeller
import it.unibo.tuprolog.argumentation.core.modules.labelling.bp.BpLabeller
import it.unibo.tuprolog.argumentation.core.modules.labelling.bp.BpPartialLabeller
import it.unibo.tuprolog.argumentation.core.modules.labelling.statement.BinaryStatementLabeller
import it.unibo.tuprolog.argumentation.core.modules.labelling.statement.PassThroughStatementLabeller
import it.unibo.tuprolog.argumentation.core.modules.labelling.statement.StatementLabeller
import it.unibo.tuprolog.argumentation.core.modules.language.Conflicts
import it.unibo.tuprolog.argumentation.core.modules.language.RuleParser
import it.unibo.tuprolog.argumentation.core.modules.language.Transposition
import it.unibo.tuprolog.argumentation.core.modules.query.ArgumentSearch
import it.unibo.tuprolog.argumentation.core.modules.query.QueryExploration
import it.unibo.tuprolog.argumentation.core.modules.query.StructuredMode
import it.unibo.tuprolog.argumentation.core.modules.utils.Debug
import it.unibo.tuprolog.argumentation.core.modules.utils.Utils

/** The modules of arg2p, by area: a solver loads each by its first call (Module::Goal) */
object Arg2pModules {
    /** The terms arg2p works on: the entities of argumentation graphs and of the rules */
    val data: List<ArgLibrary> = listOf(ArgumentTerm, AttackTerm, SupportTerm, LabelTerm, StatementTerm, RuleTerm)

    /** The rules of a theory, as the graph builders read them */
    val language: List<ArgLibrary> = listOf(RuleParser, Transposition, Conflicts)

    /** Building the argumentation graph, and changing it with the graph extensions */
    val graph: List<ArgLibrary> =
        listOf(
            AbstractMode,
            GraphStore,
            ArgumentationGraphBuilder,
            ConflictArguments,
            AttackRelation,
            AttackCandidates,
            SuperiorityRelation,
            AttackRestrictionHandler,
            BpMetaGraphHandler,
            DefeasiblePreferencesHandler,
            GenericDefeasiblePreferencesHandler,
            StrictPreferencesHandler,
        )

    /** The semantics labelling the arguments, and the statements from them */
    val labellings: List<ArgLibrary> =
        listOf(
            GroundedLabellerOptimized,
            GroundedLabeller,
            CompleteLabeller,
            PreferredLabeller,
            SemistableLabeller,
            StableLabeller,
            IdealLabeller,
            EagerLabeller,
            NaiveLabeller,
            StageLabeller,
            Cf2Labeller,
            Stage2Labeller,
            ConflictFreeLabeller,
            AdmissibleLabeller,
            StronglyAdmissibleLabeller,
            SccLabelling,
            BpLabeller,
            BpPartialLabeller,
            BpCompleteLabeller,
            StatementLabeller,
            PassThroughStatementLabeller,
            BinaryStatementLabeller,
        )

    /** Answering a query with the part of the graph it depends on */
    val query: List<ArgLibrary> = listOf(StructuredMode, QueryExploration, ArgumentSearch)

    /** The entry points, and what they rely on */
    val support: List<ArgLibrary> = listOf(EngineInterface, Utils, Debug, MetaInterpreter, ModuleCalls)

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
