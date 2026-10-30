package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.ClauseSource
import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Indicator
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.unify.Unificator

/**
 * The clauses of the program of a resolution (arg2p): those of the libraries, and, where the theory of the user is
 * visible (see [PrologExecutionContext.theoryVisible]), those of the static and the dynamic theory. A predicate the
 * theory defines is the one of the theory, whatever the libraries define under the same name: the theory of the user
 * is a module of its own, the names of the libraries do not reach it, and the modules see it only through `user::`.
 */
internal class ProgramClauses private constructor(
    private val libraries: ClauseSource,
    private val theories: List<TheoryClauseSource>,
) : ClauseSource {
    override fun clausesFor(
        goal: Struct,
        unificator: Unificator,
    ): List<Clause> {
        if (theories.none { it.defines(goal) }) return libraries.clausesFor(goal, unificator)
        // the clauses of a single theory, as it gives them; those of both, merged
        var clauses: List<Clause> = emptyList()
        var merged: ArrayList<Clause>? = null
        for (theory in theories) {
            val found = theory.clausesFor(goal, unificator)
            when {
                found.isEmpty() -> continue
                clauses.isEmpty() -> clauses = found
                merged == null -> merged = ArrayList(clauses).also { it.addAll(found) }.also { clauses = it }
                else -> merged.addAll(found)
            }
        }
        return clauses
    }

    companion object {
        // The theories of a resolution where the theory of the user is not visible: none
        private val NONE: Theory = Theory.empty()

        /**
         * The clauses of the program of [context], with the [libraries] given (its own by default), and the theory of
         * the user if [theory] (where [context] sees it by default), as the indexes of its solver keep them (see
         * [TheoryIndexes.program])
         */
        fun of(
            context: PrologExecutionContext,
            libraries: Runtime = context.libraries,
            theory: Boolean = context.theoryVisible,
        ): ProgramClauses =
            with(context) {
                val staticKb = if (theory) staticKb else NONE
                val dynamicKb = if (theory) dynamicKb else NONE
                theoryIndexes.program(libraries, staticKb, dynamicKb) {
                    ProgramClauses(
                        theoryIndexes.of(libraries.asTheory(unificator)),
                        if (theory) listOf(theory(staticKb, theoryIndexes), theory(dynamicKb, theoryIndexes)) else emptyList(),
                    )
                }
            }

        private fun theory(
            theory: Theory,
            indexes: TheoryIndexes,
        ): TheoryClauseSource = if (theory.isMutable) MutableTheoryClauses(theory) else indexes.of(theory)
    }
}

/** The clauses of a theory, which tells the predicates it defines */
internal interface TheoryClauseSource : ClauseSource {
    /** Whether the theory has clauses for the predicate of [goal] (whether they match it or not) */
    fun defines(goal: Struct): Boolean
}

/** The clauses of a mutable theory, as it is when looked up */
internal class MutableTheoryClauses(
    private val theory: Theory,
) : TheoryClauseSource {
    override fun clausesFor(
        goal: Struct,
        unificator: Unificator,
    ): List<Clause> = theory[goal].toList()

    override fun defines(goal: Struct): Boolean = theory.contains(Indicator.of(goal.functor, goal.arity))
}
