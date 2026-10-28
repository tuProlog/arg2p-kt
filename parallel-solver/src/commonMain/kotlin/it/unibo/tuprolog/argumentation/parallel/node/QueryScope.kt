package it.unibo.tuprolog.argumentation.parallel.node

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.unify.Unificator

/**
 * Query mode for a distributed solver: the rules of [theory] (converted with [settings], as the partitioner converts
 * them) the labels of the arguments for some statements may depend on, so that the workers build the arguments of
 * those rules alone. It is the exploration of query mode (see the modules structuredPartial and queryExploration),
 * on the rules rather than on the arguments, before any argument is built: the statements of the query, those of the
 * premises of their rules, those of their possible attackers (conflicting with their conclusions or their rules,
 * undercutting their rules, the contraries of their weak premises) and those the graph extensions relate them to,
 * recursively, for the statements some rule concludes; with the connected locality, the statements whose arguments
 * they may attack or support as well.
 *
 * On the part of the graph those rules build the labels of the arguments for the statements are those of the whole
 * graph, as the semantics declares (locality/1). None is found (null) when they depend on the whole graph: no query
 * mode, a semantics with no locality, a builder of the graph other than ruleGraph, a graph extension whose
 * dependencies the rules do not tell, metaConflicts (the arguments the conflicts make are derived from the whole
 * graph), or too many statements to explore.
 */
internal class QueryScope(
    theory: String,
    settings: FlagsBuilder,
) {
    private val solver = Arg2pSolverFactory.default(theory = theory, settings = settings.create())
    private val unificator = Unificator.default

    private fun goal(text: String) = Goal(text, solver.operators)

    private fun holds(goal: String) = solver.solve(Struct.parse(goal, solver.operators)).first().isYes

    init {
        solver.solve(Struct.parse("context_reset, parser:::convertAllRules(_)", solver.operators)).first()
    }

    private val extensions =
        solver.solutions(Struct.parse("setting(graphExtension, X)", solver.operators)).map { it.getValue("X").toString() }.toSet()

    // directional, connected, or null for the whole graph
    private val locality: String? =
        solver
            .solutions(
                Struct.parse(
                    "setting(queryMode), structuredWhole::builder(B), B == ruleGraph, setting(argumentLabellingMode, S), S::locality(L)",
                    solver.operators,
                ),
            ).firstOrNull()
            ?.getValue("L")
            ?.toString()
            ?.takeIf { it in setOf("directional", "connected") && (extensions - KNOWN).isEmpty() && !holds("setting(metaConflicts)") }

    private val burdens = "bp" in extensions
    private val preferences = "defeasiblePref" in extensions || "defeasibleAllPref" in extensions

    // The rules (and premises) concluding a statement S, with their premises
    private val rules = goal("(parser::classicRule(Id, Body, [S]) ; parser::classicRule(Id, [S]), Body = [])")

    // The statements whose arguments may attack those for S, or those of the rule Id (A)
    private val attackers = goal("(conflict::guarded(X, [S], _) ; conflict::unguarded(X, [S])), member(A, X)")
    private val ruleAttackers =
        goal(
            "(A = undercut(Id) ; " +
                "(conflict::guarded(X, [Id], _) ; conflict::unguarded(X, [Id])), member(A, X))",
        )

    // The statements whose arguments those for S may attack or support (D)
    private val dependants =
        goal(
            "(" +
                "(conflict::guarded([S], [Y], _) ; conflict::unguarded([S], [Y]) ; S = undercut(Y)), " +
                "(D = Y ; parser::classicRule(Y, _, [D])) ; " +
                "parser::ruleUsing(~(S), D) ; parser::ruleUsing(S, D)" +
                ")",
        )
    private val conclusions = goal("(parser::classicRule(Id, _, [D]) ; parser::classicRule(Id, [D]))")

    /** The identifiers of the rules the labels of the arguments for [statements] depend on, null for all of them */
    fun rules(statements: List<Term>): Set<String>? {
        val connected = locality ?: return null
        val found = mutableSetOf<String>()
        val explored = mutableListOf<Term>()
        val pending = ArrayDeque(statements)
        while (pending.isNotEmpty()) {
            val statement = pending.removeFirst()
            if (explored.any { covers(it, statement) }) continue
            if (explored.size == LIMIT) return null
            explored += statement
            // a statement no rule concludes has no argument: nothing depends on it
            val concluding = solver.solutions(rules.with("S" to statement))
            if (concluding.isEmpty()) continue
            val next = mutableListOf<Term>()
            concluding.forEach { rule ->
                val id = rule.getValue("Id")
                found += id.toString()
                rule.getValue("Body").castToList().toList().forEach { premise ->
                    when {
                        premise is Struct && premise.functor == "~" && premise.arity == 1 -> next += premise[0]
                        premise is Struct && premise.functor == "prolog" && premise.arity == 1 -> Unit
                        else -> next += premise
                    }
                }
                solver.solutions(ruleAttackers.with("Id" to id)).forEach { next += it.getValue("A") }
                if (preferences) next += listOf(Struct.of("sup", id, Var.anonymous()), Struct.of("sup", Var.anonymous(), id))
            }
            solver.solutions(attackers.with("S" to statement)).forEach { next += it.getValue("A") }
            if (burdens) next += Struct.of("bp", statement)
            if (connected == "connected") next += dependantsOf(statement)
            pending += next
        }
        return found
    }

    // The statements whose arguments those for [statement] may attack or support, or relate to by the extensions
    private fun dependantsOf(statement: Term): List<Term> {
        val found = solver.solutions(dependants.with("S" to statement)).map { it.getValue("D") }.toMutableList()
        if (statement is Struct && burdens && statement.functor == "bp" && statement.arity == 1) found += statement[0]
        if (statement is Struct && preferences && statement.functor == "sup" && statement.arity == 2) {
            statement.args.forEach { rule -> solver.solutions(conclusions.with("Id" to rule)).forEach { found += it.getValue("D") } }
        }
        return found
    }

    // [general] covers [specific]: every instance of the second is one of the first
    private fun covers(
        general: Term,
        specific: Term,
    ): Boolean {
        val frozen =
            specific.apply(
                Substitution.of(
                    specific.variables
                        .distinct()
                        .mapIndexed { i, v -> v to Atom.of("\$frozen_$i") }
                        .toMap(),
                ),
            )
        return unificator.match(general, frozen)
    }

    private companion object {
        // The graph extensions whose dependencies the rules tell (none, or those above)
        val KNOWN = setOf("standardPref", "rebutRestriction", "bp", "defeasiblePref", "defeasibleAllPref")

        // The statements explored at most: beyond, the whole graph (the rules may make infinitely many)
        const val LIMIT = 2_000
    }
}
