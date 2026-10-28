package it.unibo.tuprolog.argumentation.behaviour

import it.unibo.tuprolog.argumentation.Arg2p
import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.Value
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/**
 * The behaviour of arg2p through its API alone: solvers with only the flags their theory states, and the values they
 * give compared up to the names of their variables
 */
object Behaviour {
    /** A solver of [theory], with only the flags the theory states */
    fun solver(theory: String): Arg2p = Arg2p.of(theory, Flags.none())

    /** The statements for [query] in [theory] are [argsIn], [argsOut] and [argsUnd], lists as Prolog text */
    fun answerQuery(
        theory: String,
        query: String,
        argsIn: String,
        argsOut: String,
        argsUnd: String,
    ) {
        val answer = solver(theory).answer(query)
        assertVariant(Value.parse(argsIn), Value.List(answer.inStatements), "in")
        assertVariant(Value.parse(argsOut), Value.List(answer.outStatements), "out")
        assertVariant(Value.parse(argsUnd), Value.List(answer.undStatements), "und")
    }

    /**
     * The first statement labelling of [theory] (buildLabelSets/3) is [argsIn], [argsOut] and [argsUnd], lists of the
     * statements as Prolog text: the solver, at that labelling
     */
    fun buildLabelSets(
        theory: String,
        argsIn: String,
        argsOut: String,
        argsUnd: String,
    ): Arg2p =
        solver(theory).also { solver ->
            val solution = solver.solve("buildLabelSets(In, Out, Und)").next()
            assertTrue(solution.isYes, solution.toString())
            assertVariant(Value.parse(argsIn), solution.value("In")!!, "in")
            assertVariant(Value.parse(argsOut), solution.value("Out")!!, "out")
            assertVariant(Value.parse(argsUnd), solution.value("Und")!!, "und")
        }

    /** The statement labellings of [theory] (buildLabelSets/3), each as the values of In, Out and Und */
    fun labellings(theory: String): List<List<Value>> =
        solver(theory)
            .solve("buildLabelSets(In, Out, Und)")
            .toList()
            .onEach { check(!it.isHalt) { it.error.toString() } }
            .filter { it.isYes }
            .map { solution -> listOf("In", "Out", "Und").map { solution.value(it)!! } }

    /** [actual] is [expected] but for the names of its variables */
    fun assertVariant(
        expected: Value,
        actual: Value,
        message: String? = null,
    ) = assertEquals(canonical(expected), canonical(actual), message)

    /** [value] as Prolog text, its variables named _V0, _V1, ... in order of appearance */
    fun canonical(value: Value): String {
        val names = mutableMapOf<String, String>()

        fun rename(value: Value): Value =
            when (value) {
                is Value.Variable -> Value.Atom(names.getOrPut(value.name) { "_V${names.size}" })
                is Value.List -> Value.List(value.items.map(::rename))
                is Value.Struct -> Value.Struct(value.functor, value.args.map(::rename))
                else -> value
            }
        return rename(value).toString()
    }

    /** Whether [a] and [b] unify (their variables apart) */
    fun unifies(
        a: Value,
        b: Value,
    ): Boolean = Unifier().unify(a.apart("L"), b.apart("R"))

    // The variables of the value renamed with [prefix], apart from those of another
    private fun Value.apart(prefix: String): Value =
        when (this) {
            is Value.Variable -> Value.Variable(prefix + name)
            is Value.List -> Value.List(items.map { it.apart(prefix) })
            is Value.Struct -> Value.Struct(functor, args.map { it.apart(prefix) })
            else -> this
        }

    // Unification of values, with the occurs check left out (as Prolog does)
    private class Unifier {
        private val bindings = mutableMapOf<String, Value>()

        private fun resolve(value: Value): Value =
            if (value is Value.Variable) bindings[value.name]?.let { resolve(it) } ?: value else value

        fun unify(
            a: Value,
            b: Value,
        ): Boolean {
            val x = resolve(a)
            val y = resolve(b)
            return when {
                x is Value.Variable -> true.also { if (x != y) bindings[x.name] = y }
                y is Value.Variable -> true.also { bindings[y.name] = x }
                x is Value.List && y is Value.List -> x.items.size == y.items.size && x.items.zip(y.items).all { (i, j) -> unify(i, j) }
                x is Value.Struct && y is Value.Struct ->
                    x.functor == y.functor && x.arity == y.arity && x.args.zip(y.args).all { (i, j) -> unify(i, j) }
                else -> x == y
            }
        }
    }
}
