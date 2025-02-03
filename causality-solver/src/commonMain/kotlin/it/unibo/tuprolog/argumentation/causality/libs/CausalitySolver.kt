package it.unibo.tuprolog.argumentation.causality.libs

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.libs.basic.FlagsBuilder
import it.unibo.tuprolog.argumentation.core.libs.language.RuleParserBase
import it.unibo.tuprolog.argumentation.core.model.Argument
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.LabelledArgument
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.unify.Unificator
import kotlin.random.Random
import kotlin.random.nextUInt
import it.unibo.tuprolog.core.List as PlList

class CausalitySolver : ArgLibrary, Loadable {
    inner class CausalitySolve : PrimitiveWithSignature {
        override val signature = Signature("solve", 2)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            fun clean(string: String) = string.replace(" ", "").replace("'", "")

            fun equalTerms(
                a: String,
                b: String,
            ) = Unificator.default.match(Term.parse(a), Term.parse(b))

            fun attackers(
                x: Argument,
                graph: Graph,
            ): List<LabelledArgument> =
                graph.attacks
                    .filter { it.target.identifier == x.identifier }
                    .map { arg -> graph.labellings.find { arg.attacker.identifier == it.argument.identifier }!! }

            fun supporters(
                x: Argument,
                graph: Graph,
            ): List<LabelledArgument> =
                graph.labellings
                    .filter { x.identifier == it.argument.identifier } +
                    attackers(x, graph).flatMap {
                            att ->
                        attackers(att.argument, graph).flatMap { supporters(it.argument, graph) }
                    }

            // solve using grounded
            fun solveFresh(kb: Theory) =
                Arg2pSolverFactory.evaluate(
                    kb.toString(asPrologText = true),
                    FlagsBuilder(graphExtensions = emptyList()),
                ).firstOrNull() ?: Graph(emptyList(), emptyList(), emptyList())

            // get support set (union of all explanations)
            fun getSupports(
                graph: Graph,
                effect: String,
            ): Sequence<Argument> =
                graph.labellings.asSequence().filter { it.label == "in" && equalTerms(it.argument.conclusion, effect) }
                    .flatMap { supporters(it.argument, graph) }
                    .filter { it.label == "in" }
                    .map { it.argument }

            fun checkIntervention(
                intervention: PlList,
                effect: String,
            ) = solveFresh(request.context.staticKb).let {
                val support = getSupports(it, effect).map { a -> a.termRepresentation() }
                val x = intervention.toList().map { int -> Clause.of(Struct.parse(":->(fk${Random.nextUInt()}, $int)")) }
                solveFresh(request.context.staticKb.plus(Theory.of(x))).let { naf ->
                    naf.labellings
                        .filter { a -> a.label == "out" || a.label == "und" }
                        .map { a -> a.argument.termRepresentation() }
                        .any { a -> support.any { b -> Unificator.default.match(a, b) } }
                }
            }

            // if exists an explanation that is refuted trough intervention (we need minimal sets of literals from L)
            // explanations are grounded chains (grounded games are equivalent to strongly admissible sets)

            val intervention: PlList = request.arguments[0].castToList() // intervention list
            val effect: String = clean(request.arguments[1].toString())

            return sequenceOf(
                request.replyWith(checkIntervention(intervention, effect)),
            )
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
