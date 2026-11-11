package it.unibo.tuprolog.argumentation.engine.vm.compile

import it.unibo.tuprolog.argumentation.engine.term.collectVariables
import it.unibo.tuprolog.argumentation.engine.vm.program.Numbers
import it.unibo.tuprolog.argumentation.engine.vm.program.Op
import it.unibo.tuprolog.argumentation.engine.vm.program.Program
import it.unibo.tuprolog.argumentation.engine.words.Words
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Cons
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var

/**
 * What the compiler compiles inline (arithmetic, the builtins on variables, the module calls it knows) and what it calls: a
 * goal the user gives clauses to, in the theory or among the clauses compiled with it, is always called.
 */
internal class Inlining(
    private val program: Program,
) {
    // The predicates the clauses being compiled along with this one define: a builtin they define is not inlined, even in
    // a clause coming before those defining it
    var defined: Set<Int> = emptySet()

    // The goal of a module call with a known module and goal, unless the clauses define the call: its arguments go in the
    // registers, no term of the goal or of the call needed
    fun moduleGoalOf(goal: Struct): Struct? {
        if (goal.arity != 2 || (goal.functor != "::" && goal.functor != ":::")) return null
        val module = goal[0]
        val called = goal[1]
        if (module !is Atom || called !is Struct || called is Cons) return null
        return if (userDefines(goal)) null else called
    }

    /**
     * Whether [goal] is `is/2` or a comparison the theory does not redefine, over expressions of variables, small
     * integers and the integer operations the machine evaluates: compiled inline (no call: the goal joins the chunk
     * of the call after it)
     */
    fun inlineArithmetic(goal: Struct): Boolean {
        if (goal.arity != 2 || userDefines(goal)) return false
        val kind = Op.COMPARISONS[goal.functor]
        if (goal.functor == "is") {
            val target = goal[0]
            return (target is Var || (target is Integer && smallInteger(target))) && expression(goal[1])
        }
        return kind != null && expression(goal[0]) && expression(goal[1])
    }

    /** The clauses compiled from now on, until the next call, come with [clauses] */
    fun definedAlongside(clauses: Iterable<Clause>) {
        defined = clauses.mapNotNullTo(HashSet()) { clause -> clause.head?.let { program.functor(it.functor, it.arity) } }
    }

    // A builtin compiled inline, on the variables where they are: a type test on a variable, == or \\== with a variable and
    // a variable or a constant, = with a variable on a side that the other does not hold (as it is a goal otherwise)
    fun inlineBuiltin(goal: Struct): Boolean {
        val shape =
            when (goal.arity) {
                1 -> goal.functor in Words.TYPE_TESTS && goal[0] is Var
                3 ->
                    when (goal.functor) {
                        "arg" -> goal[0].let { it is Integer && smallInteger(it) } && goal[1] is Var && simple(goal[2])
                        "functor" -> goal[0] is Var && simple(goal[1]) && simple(goal[2])
                        else -> false
                    }
                2 ->
                    when (goal.functor) {
                        "==", "\\==" -> (goal[0] is Var || goal[1] is Var) && simple(goal[0]) && simple(goal[1])
                        "=" -> unifiesInline(goal[0], goal[1]) || unifiesInline(goal[1], goal[0])
                        else -> false
                    }
                else -> false
            }
        return shape && !userDefines(goal)
    }

    fun simple(term: Term): Boolean = term is Var || term is Atom || term !is Struct

    fun unifiesInline(
        variable: Term,
        other: Term,
    ): Boolean = variable is Var && (other is Var || variable !in LinkedHashSet<Var>().also { other.collectVariables(it) })

    fun expression(term: Term): Boolean =
        when (term) {
            is Var -> true
            is Integer -> smallInteger(term)
            is Struct -> operation(term) != 0 && (0 until term.arity).all { expression(term[it]) }
            else -> false
        }

    fun smallInteger(term: Integer): Boolean = !Numbers.needsBlock(term)

    fun operation(term: Struct): Int = Op.OPERATIONS.firstOrNull { it.name == term.functor && it.arity == term.arity }?.kind ?: 0

    // Whether the theory of the user has clauses for the functor of [term], a control construct: it is then called
    // as a goal, for the clauses of a library predicate to be followed by the theory's (the strict rules of an
    // argumentation theory are clauses of ->/2). Told when the clause is compiled.
    fun definedByTheory(term: Struct): Boolean {
        val functor = program.functor(term.functor, term.arity)
        return program.theory.predicate(functor) != null || functor in defined
    }

    // Whether the user gives clauses to the predicate of [goal]: the theory, or the clauses being compiled with it ([defined]).
    // A goal the user defines is called, not compiled inline
    fun userDefines(goal: Struct): Boolean = definedByTheory(goal)
}
