package it.unibo.tuprolog.argumentation.core.libraries

import it.unibo.tuprolog.argumentation.core.libraries.arg2p.Arg2pInterfaceLibrary
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.pipeline.PipelineLibrary
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.statementAcceptance.StatementAcceptanceLibrary
import it.unibo.tuprolog.argumentation.core.libraries.framework.parser.ParserLibrary
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.StoreLibrary
import it.unibo.tuprolog.argumentation.core.libraries.framework.utils.UtilsLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.abstractGraph.AbstractFrameworkGraphLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.argument.argumentBuilder.ArgumentBuilderLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.argument.argumentSearch.ArgumentSearchLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.attack.attackBuilder.AttackBuilderLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.attack.attackRelation.AttackRelationLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.attack.attackSearch.AttackSearchLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.bp.BpLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.conflictArguments.ConflictArgumentsLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.constraints.ConstraintsLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.preference.abaPref.AbaPrefLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.preference.defeasibleAllPref.DefeasibleAllPrefLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.preference.defeasiblePref.DefeasiblePrefLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.preference.standardPref.StandardPrefLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.preference.superiority.SuperiorityLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.rebutRestriction.RebutRestrictionLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.queryGraph.QueryGraphLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.ruleGraph.RuleGraphLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.admissible.AdmissibleLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.bp.bpGrounded.BpGroundedLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.bp.bpGroundedComplete.BpGroundedCompleteLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.bp.bpGroundedPartial.BpGroundedPartialLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.cf2.Cf2Library
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.complete.CompleteLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.conflictFree.ConflictFreeLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.eager.EagerLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.grounded.GroundedLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.ideal.IdealLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.labelSets.LabelSetsLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.naive.NaiveLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.preferred.PreferredLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.scc.SccLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.semiStable.SemiStableLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.stable.StableLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.stage.StageLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.stage2.Stage2Library
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.stronglyAdmissible.StronglyAdmissibleLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.statement.statement.StatementLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.statement.statementBinary.StatementBinaryLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.statement.statementPassThrough.StatementPassThroughLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.abaLanguage.AbaLanguageLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.abstractLanguage.AbstractFrameworkLanguageLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.aspicLanguage.AspicLanguageLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.conflict.ConflictLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.defeasibleLanguage.DefeasibleLanguageLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.metaRules.MetaRulesLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.moduleCalls.ModuleCallsLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.naturalTerms.NaturalTermsLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.plainProlog.PlainPrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.ruleNotation.RuleNotationLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.temporal.TemporalLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.transposition.TranspositionLibrary

/** The modules of arg2p, by area: a solver loads each by its first call (Module::Goal) */
object LibraryRegistry {
    /**
     * What is read of a theory: its rules, as its languages write them, and how the graph builders read them. In the order the libraries reading
     * a theory contribute (see parser::extension/1): what is written first, transposition building on the strict rules
     */
    val language: List<Loadable> =
        listOf(
            RuleNotationLibrary,
            PlainPrologLibrary,
            NaturalTermsLibrary,
            TranspositionLibrary,
            TemporalLibrary,
            MetaRulesLibrary,
            AspicLanguageLibrary,
            AbaLanguageLibrary,
            DefeasibleLanguageLibrary,
            AbstractFrameworkLanguageLibrary,
            ConflictLibrary,
            ModuleCallsLibrary,
        )

    /**
     * What every module stands on: the parser, through which a theory goes in (as the extensions of its language read it),
     * the store, keeping what it reads and the argumentation graph and its labels, read and changed by all the others, with
     * the terms of the graph, and the helpers on lists and terms
     */
    val framework: List<Loadable> = listOf(UtilsLibrary, ParserLibrary, StoreLibrary)

    /**
     * Building the argumentation graph: from the rules of a theory (its arguments, forward or from a query, the conflicts
     * between statements, the attacks, the preferences among arguments), or from the arguments and attacks it states
     */
    val graph: List<Loadable> =
        listOf(
            RuleGraphLibrary,
            QueryGraphLibrary,
            ArgumentBuilderLibrary,
            ArgumentSearchLibrary,
            AttackBuilderLibrary,
            AttackRelationLibrary,
            AttackSearchLibrary,
            AbstractFrameworkGraphLibrary,
        )

    /**
     * Changing the argumentation graph: the graph extensions, the preference modes among them, and the superiority of
     * arguments they weigh (which the labellings with burdens of persuasion weigh as well)
     */
    val extensions: List<Loadable> =
        listOf(
            ConflictArgumentsLibrary,
            RebutRestrictionLibrary,
            BpLibrary,
            ConstraintsLibrary,
            StandardPrefLibrary,
            DefeasiblePrefLibrary,
            DefeasibleAllPrefLibrary,
            AbaPrefLibrary,
            SuperiorityLibrary,
        )

    /**
     * Evaluating a theory: the pipeline each evaluation goes through, on the whole graph or the part a query depends on,
     * and the acceptance of a statement
     */
    val evaluation: List<Loadable> =
        listOf(
            PipelineLibrary,
            StatementAcceptanceLibrary,
        )

    /** The semantics labelling the arguments, what they share, and the statements from them */
    val labellings: List<Loadable> =
        listOf(
            LabelSetsLibrary,
            GroundedLibrary,
            CompleteLibrary,
            PreferredLibrary,
            SemiStableLibrary,
            StableLibrary,
            IdealLibrary,
            EagerLibrary,
            NaiveLibrary,
            StageLibrary,
            Cf2Library,
            Stage2Library,
            ConflictFreeLibrary,
            AdmissibleLibrary,
            StronglyAdmissibleLibrary,
            SccLibrary,
            BpGroundedLibrary,
            BpGroundedPartialLibrary,
            BpGroundedCompleteLibrary,
            StatementLibrary,
            StatementPassThroughLibrary,
            StatementBinaryLibrary,
        )

    /** The entry points (and the printing of the graph and its labels, for buildLabelSets/0) */
    val entry: List<Loadable> = listOf(Arg2pInterfaceLibrary)

    /** All the modules, each with an identifier of its own: a module is loaded by its identifier */
    val all: List<Loadable> by lazy {
        (entry + framework + language + graph + extensions + evaluation + labellings).also { modules ->
            val repeated =
                modules
                    .groupBy { it.identifier() }
                    .filterValues { it.size > 1 }
                    .keys
            require(repeated.isEmpty()) { "Modules with the same identifier: $repeated" }
        }
    }
}
