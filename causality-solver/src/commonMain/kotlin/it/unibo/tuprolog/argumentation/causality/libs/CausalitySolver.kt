package it.unibo.tuprolog.argumentation.causality.libs

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.libs.basic.FlagsBuilder
import it.unibo.tuprolog.argumentation.core.libs.language.RuleParserBase
import it.unibo.tuprolog.argumentation.core.mining.graph
import it.unibo.tuprolog.argumentation.core.model.Argument
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.LabelledArgument
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.classic.ClassicSolverFactory
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.TrackVariables
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import it.unibo.tuprolog.unify.Unificator
import kotlin.random.Random
import kotlin.random.nextUInt

class CausalitySolver : ArgLibrary, Loadable {
    inner class CausalitySolve : PrimitiveWithSignature {
        override val signature = Signature("solve", 2)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val cause: Term = request.arguments[0]
            val goal: Term = request.arguments[1]

            fun checkCause(x: Argument, cause: String) : Boolean =
                if (x.conclusion == cause) true
                else x.supports.map { checkCause(it, cause) }.any()

            fun attackers(x: Argument, graph : Graph) : List<LabelledArgument> =
                graph.attacks
                    .filter { it.target.identifier == x.identifier }
                    .map { arg -> graph.labellings.find { arg.attacker.identifier == it.argument.identifier }!! }

            fun opponents(x: Argument, graph : Graph) : List<LabelledArgument> =
                attackers(x, graph).let { a ->
                    a + a.flatMap { att -> attackers(att.argument, graph).flatMap { opponents(it.argument, graph) } }
                }

            fun supporters(x: Argument, graph : Graph) : List<LabelledArgument> =
                graph.labellings
                    .filter { x.identifier == it.argument.identifier } +
                        attackers(x, graph).flatMap {
                            att -> attackers(att.argument, graph).flatMap { supporters(it.argument, graph) }
                        }

            fun complement(t: Term) : String =
                t.toString().replace(" ", "").let {
                    if (it.startsWith("-")) it.drop(1)
                    else "-$it"
                }

            fun remove(term: Term, kb : Theory) : Theory =
                Theory.of(kb.filterNot { Unificator.default.match(it.head!!.asTerm(),
                    Struct.parse(":->(_, $term)")) }).plus(Clause.of(Struct.parse(":->(fk${Random.nextUInt()}, ${complement(term)})")))

            fun solveFresh(kb: Theory) : Graph =
                arg2pScope {
                    ClassicSolverFactory.mutableSolverWithDefaultBuiltins(
                        otherLibraries = Arg2pSolver.default(listOf(FlagsBuilder(graphExtensions = emptyList()).create())).to2pLibraries(),
                        flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL).set(TrackVariables, TrackVariables.ON),
                        staticKb = kb
                    ).let { solver ->
                        solver.solve(Struct.parse("buildLabelSetsSilent") and "context_active"(X))
                            .filter { it.isYes }
                            .map { it.substitution[X]!!.toString().toInt() }
                            .map { context -> solver.graph(context) }
                            .firstOrNull() ?: Graph(emptyList(), emptyList(), emptyList())
                    }
                }

            //  1a. check justifications (new semantics - temporarily use grounded), or
            fun checkJustification(graph : Graph) : Boolean =
                graph.labellings.asSequence().filter { it.label == "in" && it.argument.conclusion == goal.toString() }
                    .flatMap { supporters(it.argument, graph) }
                    .filter { it.label == "in" }
                    .map { checkCause(it.argument, cause.toString()) }
                    .any { it }

            //  2b. check if its intervention (graph built on rule base + negation) refutes the goal
            fun checkRefutation(graph : Graph) : Boolean =
                graph.labellings.asSequence().filter { it.label == "out" && it.argument.conclusion == goal.toString() }
                    .flatMap { opponents(it.argument, graph) }
                    .filter { it.label == "in" }
                    .map { checkCause(it.argument, complement(cause)) }
                    .any { it }

            return sequenceOf(request.replyWith(
                checkJustification(solveFresh(request.context.staticKb)) ||
                        checkRefutation(solveFresh(remove(cause, request.context.staticKb)))))
        }
    }

    override val alias = "prolog.argumentation.causality.solver"

    override val baseContent: Library
        get() =
            listOf(CausalitySolve()).let { primitives ->
                Library.of(
                    alias = this.alias,
                    primitives = primitives.associateBy { it.signature },
                    operators = RuleParserBase.operators(),
                )
            }

    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier(): String = "causality"

    override var theoryOperators =
        RuleParserBase.operators()
            .plus(OperatorSet.DEFAULT)
}
