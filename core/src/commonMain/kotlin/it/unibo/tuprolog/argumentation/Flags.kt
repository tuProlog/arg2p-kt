@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.argumentation.core.flags.ArgumentLabellingMode
import it.unibo.tuprolog.argumentation.core.flags.AutoTransposition
import it.unibo.tuprolog.argumentation.core.flags.DebugOutput
import it.unibo.tuprolog.argumentation.core.flags.GraphBuildMode
import it.unibo.tuprolog.argumentation.core.flags.MetaConflicts
import it.unibo.tuprolog.argumentation.core.flags.MetaRules
import it.unibo.tuprolog.argumentation.core.flags.ModulesPath
import it.unibo.tuprolog.argumentation.core.flags.PrologStrictCompatibility
import it.unibo.tuprolog.argumentation.core.flags.QueryMode
import it.unibo.tuprolog.argumentation.core.flags.StatementLabellingMode
import it.unibo.tuprolog.argumentation.core.flags.Temporal
import it.unibo.tuprolog.argumentation.core.model.terms.TermValues
import it.unibo.tuprolog.argumentation.core.modules.ArgLibrary
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import kotlin.js.JsExport
import kotlin.js.JsName
import kotlin.js.JsStatic
import kotlin.jvm.JvmStatic
import it.unibo.tuprolog.argumentation.core.flags.ConflictModel as ConflictModelFlag
import it.unibo.tuprolog.argumentation.core.flags.GraphBuilder as GraphBuilderFlag
import it.unibo.tuprolog.argumentation.core.flags.GraphExtension as GraphExtensionFlag
import it.unibo.tuprolog.argumentation.core.flags.GraphLanguage as GraphLanguageFlag
import it.unibo.tuprolog.argumentation.core.flags.OrderingComparator as OrderingComparatorFlag
import it.unibo.tuprolog.argumentation.core.flags.OrderingPrinciple as OrderingPrincipleFlag

/**
 * The flags of a solver (see the flags reference of arg2p), as the facts stating them: [defaults] (those of arg2p),
 * [none] (only those the theory states), or [parse] (as Prolog text). A flag set replaces what the flags stated for
 * it; the theory may state flags as well, which override these. The profile of the theory ([graphBuildMode]) presets
 * the flags its definition needs.
 */
@JsExport
class Flags private constructor(
    // the facts stating each flag, by its predicate, in the order the flags were stated
    private val facts: MutableMap<String, List<String>>,
) {
    /** The profile of the theory */
    fun graphBuildMode(profile: Profile): Flags = value(GraphBuildMode, profile.value)

    /** The semantics of the argument labelling */
    fun argumentLabellingMode(semantics: Semantics): Flags = value(ArgumentLabellingMode, semantics.value)

    /** The labelling of the statements */
    fun statementLabellingMode(labelling: StatementLabelling): Flags = value(StatementLabellingMode, labelling.value)

    /** The ordering principle of the preferences */
    fun orderingPrinciple(principle: OrderingPrinciple): Flags = value(OrderingPrincipleFlag, principle.value)

    /** The comparator of the preferences */
    fun orderingComparator(comparator: OrderingComparator): Flags = value(OrderingComparatorFlag, comparator.value)

    /** The extensions of the graph: none for no extension (an array in JavaScript) */
    fun graphExtensions(vararg extensions: GraphExtension): Flags =
        values(GraphExtensionFlag, extensions.map { it.value }.ifEmpty { listOf("none") })

    /** The languages extending the one of arg2p: none for no language (an array in JavaScript) */
    fun graphLanguage(vararg languages: GraphLanguage): Flags =
        values(GraphLanguageFlag, languages.map { it.value }.ifEmpty { listOf("none") })

    /** The module building the graph */
    fun graphBuilder(builder: GraphBuilder): Flags = value(GraphBuilderFlag, builder.value)

    /** The conflicts beyond those of the languages of the theory: a part of them, or all of them (builtin) */
    fun conflictModel(model: ConflictModel): Flags = value(ConflictModelFlag, model.value)

    /**
     * The conflicts beyond those of the languages of the theory: the parts of them, none for none but those of the
     * languages (an array in JavaScript, conflictModels)
     */
    @JsName("conflictModels")
    fun conflictModel(vararg models: ConflictModel): Flags = values(ConflictModelFlag, models.map { it.value }.ifEmpty { listOf("none") })

    /** Where the modules loaded by name are looked for */
    fun modulesPath(path: String): Flags = value(ModulesPath, path)

    /** Whether the graph is built on the arguments the query depends on alone */
    fun queryMode(on: Boolean): Flags = switch(QueryMode, on)

    /** Whether the strict rules are transposed */
    fun autoTransposition(on: Boolean): Flags = switch(AutoTransposition, on)

    /** Whether the theory is read as strict Prolog (no rules of arg2p) */
    fun prologStrictCompatibility(on: Boolean): Flags = switch(PrologStrictCompatibility, on)

    /** Whether the conflicts are arguments as well */
    fun metaConflicts(on: Boolean): Flags = switch(MetaConflicts, on)

    /** Whether the rules are arguments as well */
    fun metaRules(on: Boolean): Flags = switch(MetaRules, on)

    /** Whether the theory has the temporal axioms (the module temporal), over regions of time */
    fun temporal(on: Boolean): Flags = switch(Temporal, on)

    /** Whether the modules print their debug output */
    fun debug(on: Boolean): Flags = switch(DebugOutput, on)

    /** The flags as the facts stating them, Prolog text */
    override fun toString(): String = facts.values.flatten().joinToString("") { "$it.\n" }

    override fun equals(other: Any?): Boolean = other is Flags && other.facts == facts

    override fun hashCode(): Int = facts.hashCode()

    /** The library of the flags, for a solver of arg2p */
    @JsExport.Ignore
    @InternalArg2pApi
    fun library(): ArgLibrary =
        object : ArgLibrary {
            override val alias = ALIAS

            override val baseContent: Library
                get() = Library.of(alias = ALIAS, clauses = theory())

            override val baseFlags: Iterable<ArgsFlag<*, *>>
                get() = emptyList()
        }

    /** The facts stating the flags, as a theory */
    internal fun theory(): Theory = Theory.parse(toString(), TermValues.operators)

    private fun value(
        flag: ArgsFlag<*, *>,
        value: String,
    ) = values(flag, listOf(value))

    private fun values(
        flag: ArgsFlag<*, *>,
        values: List<String>,
    ) = apply { facts[flag.predicate()] = values.map { "${flag.predicate()}(${Value.Atom(it)})" } }

    private fun switch(
        flag: ArgsFlag<*, *>,
        on: Boolean,
    ) = apply { if (on) facts[flag.predicate()] = listOf(flag.predicate()) else facts.remove(flag.predicate()) }

    companion object {
        private const val ALIAS = "prolog.argumentation.flags"

        /** The flags of arg2p by default (those of [FlagsBuilder]) */
        @JvmStatic
        @JsStatic
        fun defaults(): Flags = of(FlagsBuilder())

        /**
         * No flags: the solver has only those the theory states, and those its profile presets (graphBuildMode, ASPIC+
         * when the theory names none); the other flags take their defaults, the switches are off
         */
        @JvmStatic
        @JsStatic
        fun none(): Flags = Flags(linkedMapOf())

        /**
         * The flags stated by the facts of [text], Prolog text (`argumentLabellingMode(preferred). queryMode.`): an
         * [Arg2pException] telling where it does not parse otherwise
         */
        @JvmStatic
        @JsStatic
        fun parse(text: String): Flags =
            Flags(
                SyntaxProblem
                    .parsing("Flags") { Theory.parse(text, TermValues.operators) }
                    .clauses
                    .map { clause -> clause.takeIf { it.isFact }?.castToFact() ?: throw Arg2pException("Flags: not a fact: $clause") }
                    .groupBy({ it.head.functor }, { TermValues.format(it.head) })
                    .toMutableMap(),
            )

        /** The flags of [builder] */
        @JsExport.Ignore
        @JvmStatic
        fun of(builder: FlagsBuilder): Flags =
            if (builder.emptyBuilder) {
                none()
            } else {
                with(builder) {
                    Flags(linkedMapOf())
                        // the builder lists the extensions in the order they apply, which is the reverse of the order
                        // they are stated in (modifyGraph applies the last stated first)
                        .values(GraphExtensionFlag, graphExtensions.toList().asReversed().ifEmpty { listOf("none") })
                        .switch(QueryMode, queryMode)
                        .switch(AutoTransposition, autoTransposition)
                        .switch(PrologStrictCompatibility, prologStrictCompatibility)
                        .switch(MetaConflicts, metaConflicts)
                        .switch(MetaRules, metaRules)
                        .value(GraphBuildMode, graphBuildMode)
                        .value(GraphBuilderFlag, graphBuilder)
                        .value(ArgumentLabellingMode, argumentLabellingMode)
                        .value(StatementLabellingMode, statementLabellingMode)
                        .value(OrderingPrincipleFlag, orderingPrinciple)
                        .value(OrderingComparatorFlag, orderingComparator)
                        .value(ModulesPath, modulesPath)
                }
            }
    }
}
