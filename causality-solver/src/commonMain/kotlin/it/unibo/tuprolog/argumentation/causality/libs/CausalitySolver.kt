package it.unibo.tuprolog.argumentation.causality.libs

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.libs.basic.DynamicLoader
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
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import it.unibo.tuprolog.unify.Unificator
import kotlin.random.Random
import kotlin.random.nextUInt
import it.unibo.tuprolog.core.List as PlList

class CausalitySolver :
    ArgLibrary,
    Loadable {
    fun clean(string: String) = string.replace(" ", "").replace("'", "")

    val solver =
        Arg2pSolverFactory.default(
            settings = FlagsBuilder(graphExtensions = emptyList(), argumentLabellingMode = "grounded").create(),
        )

    val operators = Arg2pSolver.default().operators()

    private fun equalTerms(
        a: String,
        b: String,
    ) = Unificator.default.match(Term.parse(a), Term.parse(b))

    private fun attackers(
        x: Argument,
        graph: Graph,
    ): List<LabelledArgument> =
        graph.attacks
            .filter { it.target.identifier == x.identifier }
            .map { arg -> graph.labellings.find { arg.attacker.identifier == it.argument.identifier }!! }

    private fun supporters(
        x: Argument,
        graph: Graph,
        chain: List<String>,
    ): List<LabelledArgument> =
        if (x.identifier in chain) {
            emptyList()
        } else {
            graph.labellings
                .filter { x.identifier == it.argument.identifier && it.label == "in" } +
                attackers(x, graph).flatMap { att ->
                    attackers(att.argument, graph)
                        .filter { it.argument.identifier != x.identifier }
                        .flatMap { supporters(it.argument, graph, chain + x.identifier) }
                }
        }

    private fun solveFresh(
        effect: String,
        kb: Theory,
    ): Graph {
        this.solver.resetStaticKb()
        this.solver.loadStaticKb(Theory.parse(kb.toString(asPrologText = true), operators))
        return this.solver
            .solve(Struct.parse("answerQuery($effect)"))
            .map { solver.graph() }
            .firstOrNull() ?: Graph(emptyList(), emptyList(), emptyList())
    }

    // get support set (union of all explanations)
    private fun getSupports(
        graph: Graph,
        effect: String,
    ): Sequence<Argument> =
        graph.labellings
            .asSequence()
            .filter { equalTerms(it.argument.conclusion, effect) }
            .flatMap { supporters(it.argument, graph, emptyList()) }
            .map { it.argument }

    private fun checkBaseTheory(
        request: Solve.Request<ExecutionContext>,
        effect: String,
    ) = solveFresh(effect, request.context.staticKb).let {
        getSupports(it, effect).map { a -> a.termRepresentation() }.toList()
    }

    // now returns IN attacker (should always be a singleton but not sure)
    private fun checkIntervention(
        request: Solve.Request<ExecutionContext>,
        intervention: PlList,
        support: List<Term>,
        effect: String,
    ) = intervention.toList().map { int -> Clause.of(Struct.parse(":->(fk${int.hashCode() and Int.MAX_VALUE}, $int)")) }.let {
        solveFresh(effect, request.context.staticKb.plus(Theory.of(it))).let { naf ->
            naf.labellings
                .filter { a -> a.label == "out" || a.label == "und" }
                .filter { a -> support.any { b -> Unificator.default.match(a.argument.termRepresentation(), b) } }
                .flatMap { a -> attackers(a.argument, naf).filter { b -> b.label == "in" } }
                .map { a -> a.argument }
                .distinct()
        }
    }

    private fun getSubsets(
        request: Solve.Request<ExecutionContext>,
        intervention: PlList,
    ) = arg2pScope {
        request
            .solve("proper_subsets"(intervention, X))
            .filter {
                it.isYes
            }.map { it.substitution[X]!!.castToList() }
            .toList()
    }

    private fun getInterventions(
        request: Solve.Request<ExecutionContext>,
        cause: Term,
        effect: String,
    ): Sequence<PlList> {
        val rules =
            Term.parse(
                solveFresh(effect, request.context.staticKb)
                    .arguments
                    .map { it.topRule }
                    .filter { it != "none" }
                    .toList()
                    .toString(),
            )

        return arg2pScope {
            request
                .solve("generate_interventions"(rules, cause, X))
                .filter {
                    it.isYes
                }.map { it.substitution[X]!!.castToList() }
        }
    }

    // if exists an explanation that is refuted trough intervention (we need minimal sets of literals from L)
    // explanations are grounded chains (grounded games are equivalent to strongly admissible sets)

    private fun checkCausalityICAIL24(
        request: Solve.Request<ExecutionContext>,
        intervention: PlList,
        effect: String,
    ): Boolean {
        val supports = checkBaseTheory(request, effect)
        if (supports.isEmpty()) return false
        val attackers = checkIntervention(request, intervention, supports, effect)
        return attackers.isNotEmpty() &&
            (
                intervention.estimatedLength <= 1 ||
                    getSubsets(request, intervention).none { checkIntervention(request, it, supports, effect).isNotEmpty() }
            )
    }

    // if exists an explanation that is refuted trough intervention (now there must exist an IN attacker) (we need minimal sets of literals from L)
    // explanations are grounded chains (grounded games are equivalent to strongly admissible sets)

    private fun checkCausalityICAIL25(
        request: Solve.Request<ExecutionContext>,
        intervention: PlList,
        effect: String,
    ): Boolean {
        val supports = checkBaseTheory(request, effect)
        if (supports.isEmpty()) return false
        val attackers = checkIntervention(request, intervention, supports, effect).map { a -> a.termRepresentation() }
        return attackers.isNotEmpty() &&
            (
                intervention.estimatedLength <= 1 ||
                    getSubsets(request, intervention).none { x ->
                        val n = checkIntervention(request, x, supports, effect).map { a -> a.termRepresentation() }
                        attackers.all { a -> n.any { b -> Unificator.default.match(a, b) } }
                    }
            )
    }

    // must exist an IN argument for the effect and after intervention there must not exist such arguments

    private fun checkButFor(
        request: Solve.Request<ExecutionContext>,
        intervention: PlList,
        effect: String,
    ): Boolean =
        solveFresh(effect, request.context.staticKb).let {
            it.labellings.any { arg -> arg.label == "in" && equalTerms(arg.argument.conclusion, effect) }
        } &&
            intervention.toList().map { int -> Clause.of(Struct.parse(":->(fk${Random.nextUInt()}, $int)")) }.let {
                solveFresh(effect, request.context.staticKb.plus(Theory.of(it))).let { naf ->
                    naf.labellings.none { arg -> arg.label == "in" && equalTerms(arg.argument.conclusion, effect) }
                }
            }

    inner class CausalityCheckCauseButFor : PrimitiveWithSignature {
        override val signature = Signature("but_for", 2)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val cause: Term = request.arguments[0]
            val effect: String = clean(request.arguments[1].toString())

            return sequenceOf(
                request.replyWith(
                    arg2pScope {
                        request
                            .solve("compl"(cause, X))
                            .filter { it.isYes }
                            .map { PlList.of(it.substitution[X]!!) }
                            .map { checkButFor(request, it, effect) }
                            .firstOrNull() ?: false
                    },
                ),
            )
        }
    }

    inner class CausalityCheckInterventionICAIL24 : PrimitiveWithSignature {
        override val signature = Signature("ness_original_intervention", 2)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val intervention: PlList = request.arguments[0].castToList()
            val effect: String = clean(request.arguments[1].toString())

            return sequenceOf(
                request.replyWith(checkCausalityICAIL24(request, intervention, effect)),
            )
        }
    }

    inner class CausalityCheckCauseICAIL24 : PrimitiveWithSignature {
        override val signature = Signature("ness_original", 3)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val intervention: Term = request.arguments[0]
            val cause: Term = request.arguments[1]
            val effect: String = clean(request.arguments[2].toString())

            if (intervention !is Var) {
                throw TypeError.forGoal(
                    request.context,
                    request.signature,
                    TypeError.Expected.VARIABLE,
                    intervention,
                )
            }

            return sequenceOf(
                request.replyWith(
                    getInterventions(request, cause, effect)
                        .filter { checkCausalityICAIL24(request, it, effect) }
                        .map { Substitution.of(intervention, it) }
                        .firstOrNull() ?: Substitution.failed(),
                ),
            )
        }
    }

    inner class CausalityCheckInterventionICAIL25 : PrimitiveWithSignature {
        override val signature = Signature("ness_intervention", 2)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val intervention: PlList = request.arguments[0].castToList()
            val effect: String = clean(request.arguments[1].toString())

            return sequenceOf(
                request.replyWith(checkCausalityICAIL25(request, intervention, effect)),
            )
        }
    }

    inner class CausalityCheckCauseICAIL25 : PrimitiveWithSignature {
        override val signature = Signature("ness", 3)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val intervention: Term = request.arguments[0]
            val cause: Term = request.arguments[1]
            val effect: String = clean(request.arguments[2].toString())

            if (intervention !is Var) {
                throw TypeError.forGoal(
                    request.context,
                    request.signature,
                    TypeError.Expected.VARIABLE,
                    intervention,
                )
            }

            return sequenceOf(
                request.replyWith(
                    getInterventions(request, cause, effect)
                        .filter { checkCausalityICAIL25(request, it, effect) }
                        .map { Substitution.of(intervention, it) }
                        .firstOrNull() ?: Substitution.failed(),
                ),
            )
        }
    }

    override val alias = "prolog.argumentation.causality.solver"

    override val baseContent: Library
        get() =
            listOf(
                CausalityCheckInterventionICAIL24(),
                CausalityCheckCauseICAIL24(),
                CausalityCheckInterventionICAIL25(),
                CausalityCheckCauseICAIL25(),
                CausalityCheckCauseButFor(),
            ).let { primitives ->
                Library.of(
                    alias = this.alias,
                    primitives = primitives.associateBy { it.signature },
                    clauses =
                        Theory.parse(
                            """
                            subsets([], []).
                            subsets([H|T], [H|NT]) :- subsets(T, NT).
                            subsets([_|T], NT) :- subsets(T, NT).
                            
                            proper_subsets(L, S) :-
                                subsets(L, S),
                                S \= [],
                                S \= L.
                            
                            generate_interventions(_, S, [CS]) :-
                                compl(S, CS).
                            generate_interventions(_, S, [undercut(S)]) :-
                                S \= -_.
                            generate_interventions(R, S, SUP) :-
                                parser:::convertAllRules(_),
                                compl(S, CS),
                                unused_premises(R, UP), !,
                                subsets(UP, SUP),
                                member(CS, SUP),
                                SUP \= [].
                            
                            compl(-X, X) :- !.
                            compl(X, -X).
                            
                            unused_premises(R, UP) :-
                                findall(P, (parser::rule(Id, P, _), \+ member(Id, R), ground(Id)), RR),
                                utils::appendLists(RR, UP).
                            
                            """.trimIndent(),
                            theoryOperators,
                        ),
                )
            }

    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier(): String = "causality"

    override var theoryOperators =
        DynamicLoader
            .operators()
            .plus(RuleParserBase.operators())
            .plus(OperatorSet.DEFAULT)
}
