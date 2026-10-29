@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.causality.modules.CausalityModule
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.mining.activeBranch
import it.unibo.tuprolog.argumentation.core.model.mining.graph
import it.unibo.tuprolog.argumentation.core.model.terms.TermValues
import it.unibo.tuprolog.argumentation.core.runtime.theory.RuleAnnotations
import it.unibo.tuprolog.argumentation.parallel.LABELLINGS
import it.unibo.tuprolog.argumentation.parallel.checked
import it.unibo.tuprolog.argumentation.parallel.graphsOf
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.channel.OutputChannel
import it.unibo.tuprolog.theory.Theory
import kotlin.js.JsExport
import kotlin.js.JsName
import kotlin.js.JsStatic
import kotlin.jvm.JvmOverloads
import kotlin.jvm.JvmStatic
import it.unibo.tuprolog.solve.Solution as LogicSolution

/**
 * A solver of arg2p for a theory, as Prolog text, with its [Flags]: it answers queries ([answer]), gives the graph of
 * each labelling ([graphs]), solves Prolog goals ([solve]) and tests causality ([causality]).
 *
 * ```kotlin
 * val arg2p = Arg2p.of("r1 : [] => a.\nr2 : [] => -a.\nsup(r1, r2).")
 * arg2p.answer("a").inStatements      // [a]
 * ```
 *
 * The context of the solver keeps a branch for every stage of a resolution (each module call made with `:::`),
 * numbered from 0: [activeBranch] is the last one, [graphAt] the graph of any of them, and [contextFacts] what a
 * branch stores.
 */
@JsExport
class Arg2p private constructor(
    private val solver: MutableSolver,
    // the libraries of the solver, which declare its flags
    private val arg2p: Arg2pSolver,
) {
    /** The causality tests on the theory: NESS and but-for (see [Causality]) */
    val causality: Causality = Causality(this)

    /** The theory, as Prolog text */
    val theory: String get() = solver.staticKb.toString(asPrologText = true)

    /**
     * The statements for [query], Prolog text, labelled in, out and und (see `answerQuery/4`)
     *
     * @throws Arg2pException if the evaluation halts (a theory its profile refuses, a query that cannot be parsed)
     */
    fun answer(query: String): Answer = AnswerQuery(term(query)).let { it.answer(first(it.goal)) }

    /**
     * The graph of each labelling of the theory: one, or several for a semantics with several labellings (preferred,
     * stable, ...)
     *
     * @throws Arg2pException if the evaluation halts (a theory its profile refuses)
     */
    @JsExport.Ignore
    @JsName("graphsList")
    fun graphs(): List<Graph> =
        // read while the solver is at each solution, the graph of its labelling in the active branch
        graphsOf(solver.solve(Struct.of(LABELLINGS)).iterator()) { solver.graph() }

    /** The graph of each labelling of the theory (an array in JavaScript) */
    @JsName("graphs")
    fun graphArray(): Array<Graph> = graphs().toTypedArray()

    /** The graph of the last resolution: the one of its last stage */
    fun graph(): Graph = graphAt(activeBranch)

    /** The graph of the context branch [branch] */
    fun graphAt(branch: Int): Graph = solver.graph(branch)

    /** The context branch of the last stage of the last resolution */
    val activeBranch: Int get() = solver.activeBranch()

    /** The flags the solver knows, by name: graphBuildMode, queryMode, ... (see [setting]) */
    @JsExport.Ignore
    @JsName("knownFlagsList")
    val knownFlags: List<String> get() = declaredFlags.keys.toList()

    /** The flags the solver knows, by name (an array in JavaScript) */
    @JsName("knownFlags")
    val knownFlagArray: Array<String> get() = knownFlags.toTypedArray()

    /**
     * The values of [flag] in effect for the theory, as the solver checks them: those the theory states, or else those
     * of the [Flags] of the solver that differ from their default, or else those the profile of the theory presets (see
     * Profiles). A switch on has the value `true`, off none.
     *
     * @throws Arg2pException for a flag the solver does not know (see [knownFlags])
     */
    @JsExport.Ignore
    @JsName("settingList")
    fun setting(flag: String): List<String> {
        val declared = declaredFlags[flag] ?: throw Arg2pException("Not a flag of arg2p: $flag (known: ${knownFlags.joinToString()})")
        val value = Var.of("Value")
        return if (declared.isSwitch) {
            listOfNotNull(first(Struct.of("setting", Atom.of(flag)))?.let { "true" })
        } else {
            solver
                .solve(Struct.of("setting", Atom.of(flag), value))
                .map(::checked)
                .filter { it.isYes }
                .map { solution -> solution.substitution[value]!!.let { (it as? Atom)?.value ?: TermValues.format(it) } }
                .toList()
        }
    }

    /** The values of [flag] in effect for the theory (an array in JavaScript) */
    @JsName("setting")
    fun settingArray(flag: String): Array<String> = setting(flag).toTypedArray()

    private val declaredFlags get() = arg2p.settings.flags

    /** The solutions of [goal], Prolog text, computed one at a time as they are asked for */
    fun solve(goal: String): Solutions = solutionsOf(solver.solve(struct(goal)).iterator())

    /**
     * What the context branch [branch] stores matching [goal], Prolog text (facts, or clause(Head, Body) for rules):
     * the solutions of context_check(Branch, Goal)
     */
    fun contextFacts(
        branch: Int,
        goal: String,
    ): Solutions = solutionsOf(solver.solve(Struct.of("context_check", Integer.of(branch), struct(goal))).iterator())

    /** Adds the clauses of [theory], Prolog text, to the theory (an [Arg2pException] telling where it does not parse) */
    fun add(theory: String) = solver.appendStaticKb(parsed(theory))

    /** Replaces the theory with [theory], Prolog text (an [Arg2pException] telling where it does not parse) */
    fun load(theory: String) = solver.loadStaticKb(parsed(theory))

    // The clauses of [theory], read with the operators of the solver
    private fun parsed(theory: String): Theory = SyntaxProblem.parsing("Theory") { RuleAnnotations.parse(theory, solver.operators) }

    // The first solution of [goal]: null if it fails, an Arg2pException if it halts
    internal fun first(goal: Struct): LogicSolution? = checked(solver.solve(goal).first()).takeIf { it.isYes }

    internal fun term(text: String): Term = SyntaxProblem.parsing("Query") { TermValues.term(text, solver.operators) }

    private fun struct(goal: String): Struct = term(goal).let { it as? Struct ?: throw Arg2pException("Not a goal: $goal") }

    /** A solver of arg2p, as it is described: its theory, its flags, and where what it prints goes */
    class Builder {
        private var theory = ""
        private var flags = Flags.defaults()
        private var output: ((String) -> Unit)? = null

        /** The theory, as Prolog text */
        fun theory(theory: String): Builder = apply { this.theory = theory }

        /** The flags ([Flags.defaults] otherwise) */
        fun flags(flags: Flags): Builder = apply { this.flags = flags }

        /** The function receiving what the solver prints (the standard output otherwise) */
        fun output(consumer: (String) -> Unit): Builder = apply { output = consumer }

        /** The solver: an [Arg2pException] telling where the theory does not parse otherwise */
        fun build(): Arg2p {
            // the causality module of its own for each solver: it keeps the graphs it computes
            val arg2p = Arg2pSolver.default(listOf(flags.library()), listOf(CausalityModule()))
            return Arg2p(
                SyntaxProblem.parsing("Theory") {
                    Arg2pSolverFactory.solverOf(arg2p, theory, output = output?.let { OutputChannel.of(it) } ?: OutputChannel.stdOut())
                },
                arg2p,
            )
        }
    }

    companion object {
        /** A solver of arg2p for [theory], Prolog text, with [flags] */
        @JvmStatic
        @JvmOverloads
        @JsStatic
        fun of(
            theory: String = "",
            flags: Flags = Flags.defaults(),
        ): Arg2p = Builder().theory(theory).flags(flags).build()

        /** A solver of arg2p, as it is described */
        @JvmStatic
        @JsStatic
        fun builder(): Builder = Builder()

        /** Where [theory], Prolog text (a theory, or flags), does not parse; null if it does. Nothing is solved. */
        @JvmStatic
        @JsStatic
        fun checkTheory(theory: String): SyntaxProblem? = SyntaxProblem.check { RuleAnnotations.parse(theory, TermValues.operators) }

        /** Where [query], a goal in Prolog text, does not parse; null if it does. Nothing is solved. */
        @JvmStatic
        @JsStatic
        fun checkQuery(query: String): SyntaxProblem? = SyntaxProblem.check { TermValues.term(query) }

        /** The version of arg2p, the released one for a release */
        @JvmStatic
        @JsStatic
        fun version(): String = Arg2pLibrary.version()
    }
}
