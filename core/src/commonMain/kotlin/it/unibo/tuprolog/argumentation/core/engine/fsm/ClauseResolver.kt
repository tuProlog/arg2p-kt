package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.argumentation.core.runtime.renamedCopy
import it.unibo.tuprolog.core.Rule
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.core.prepareForExecution
import it.unibo.tuprolog.unify.Unificator

/**
 * The resolution of a goal with a clause: the bindings it gives the variables of the goal (those of the clause are
 * renamed apart, and only occur in its body), and the body to execute in place of the goal, prepared for execution.
 */
internal class Resolution(
    val goalBindings: Substitution.Unifier,
    val body: Term,
) {
    /** [substitution], the one of the resolution, extended with the bindings of the goal (see [extendedWith]) */
    fun extend(substitution: Substitution.Unifier): Substitution.Unifier = substitution.extendedWith(goalBindings)
}

/** How the engine resolves a goal with one of the clauses selected for it (arg2p) */
internal interface ClauseResolver {
    /** The resolution of [goal] with [rule], as stored in its theory; null if the head of [rule] does not unify with [goal] */
    fun resolve(
        rule: Rule,
        goal: Term,
    ): Resolution?

    companion object {
        /**
         * The resolver for the unificator of a resolution: the one of arg2p resolves the clauses as stored
         * ([StoredClauseResolver]), any other one renamed ([RenamingClauseResolver])
         */
        fun of(unificator: Unificator): ClauseResolver =
            if (unificator === Arg2pUnificator) StoredClauseResolver else RenamingClauseResolver(unificator)
    }
}

/**
 * Resolution as in tuProlog, through any [unificator]: the clause renamed as a whole, its head unified with the goal
 * (the head first, so that a variable of the clause meeting one of the goal is bound to it), its body prepared for
 * execution and built with the unifier.
 */
internal class RenamingClauseResolver(
    private val unificator: Unificator,
) : ClauseResolver {
    override fun resolve(
        rule: Rule,
        goal: Term,
    ): Resolution? {
        val renamed = rule.renamedCopy()
        val substitution = unificator.mgu(renamed.head, goal)
        if (!substitution.isSuccess) return null
        val unifier = substitution.castToUnifier()
        val ruleVariables = HashSet<Var>().also { renamed.head.collectVariables(it) }
        val goalBindings = unifier.filter { (variable, _) -> variable !in ruleVariables }.castToUnifier()
        return Resolution(goalBindings, renamed.prepareForExecution(unifier).body[unifier])
    }
}
