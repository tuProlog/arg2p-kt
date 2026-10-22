package it.unibo.tuprolog.argumentation.core.libs

import it.unibo.tuprolog.argumentation.core.libs.basic.EngineInterface
import it.unibo.tuprolog.argumentation.core.libs.data.ArgumentTerm
import it.unibo.tuprolog.argumentation.core.libs.extra.MetaInterpreter
import it.unibo.tuprolog.argumentation.core.libs.extra.ModuleCalls
import it.unibo.tuprolog.argumentation.core.libs.graph.AbstractMode
import it.unibo.tuprolog.argumentation.core.libs.graph.attack.AttackCandidates
import it.unibo.tuprolog.argumentation.core.libs.graph.attack.AttackRelation
import it.unibo.tuprolog.argumentation.core.libs.graph.builder.ArgumentationGraphBuilder
import it.unibo.tuprolog.argumentation.core.libs.graph.builder.ConflictArguments
import it.unibo.tuprolog.argumentation.core.libs.graph.extension.AttackRestrictionHandler
import it.unibo.tuprolog.argumentation.core.libs.graph.extension.BpMetaGraphHandler
import it.unibo.tuprolog.argumentation.core.libs.graph.extension.DefeasiblePreferencesHandler
import it.unibo.tuprolog.argumentation.core.libs.graph.extension.GenericDefeasiblePreferencesHandler
import it.unibo.tuprolog.argumentation.core.libs.graph.extension.StrictPreferencesHandler
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.AdmissibleLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.BinaryStatementLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.BpCompleteLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.BpLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.BpPartialLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.Cf2Labeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.CompleteLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.ConflictFreeLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.EagerLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.GroundedLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.GroundedLabellerOptimized
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.IdealLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.NaiveLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.PassThroughStatementLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.PreferredLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.SccLabelling
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.SemistableLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.StableLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.Stage2Labeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.StageLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.StatementLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.StronglyAdmissibleLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.store.GraphStore
import it.unibo.tuprolog.argumentation.core.libs.language.Conflicts
import it.unibo.tuprolog.argumentation.core.libs.language.RuleParser
import it.unibo.tuprolog.argumentation.core.libs.structured.StructuredMode
import it.unibo.tuprolog.argumentation.core.libs.utils.Debug
import it.unibo.tuprolog.argumentation.core.libs.utils.SuperiorityRelation
import it.unibo.tuprolog.argumentation.core.libs.utils.Utils

/** The modules of arg2p, by area: a solver loads each by its first call (Module::Goal) */
object Arg2pModules {
    /** The terms arg2p works on */
    val data: List<ArgLibrary> = listOf(ArgumentTerm)

    /** The rules of a theory, as the graph builders read them */
    val language: List<ArgLibrary> = listOf(RuleParser, Conflicts)

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
    val query: List<ArgLibrary> = listOf(StructuredMode)

    /** The entry points, and what they rely on */
    val support: List<ArgLibrary> = listOf(EngineInterface, Utils, Debug, MetaInterpreter, ModuleCalls)

    val all: List<ArgLibrary>
        get() = support + data + language + graph + labellings + query
}
