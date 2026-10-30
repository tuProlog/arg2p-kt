package it.unibo.tuprolog.argumentation.core.modules.graph.extension

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

// Arguments holding for sets of values ({ } constraints) and their instances (graphExtension(constraints)), in the
// whole graph and in query mode
class ConstraintsModuleTest {
    private val flags = "graphExtension(standardPref).\ngraphExtension(constraints).\n"

    // f holds after 1; a time after it is supported by its instance, a time before it by nothing
    private val after = "f1 :=> s.\nr0 : s, {T > 1} => holdsAt(f, T).\nr1 : holdsAt(f, 5) => ok.\nr2 : holdsAt(f, 0) => no.\n"

    // -f stated at 7: it attacks the argument for every time after 1, and its instance for 7, not those for 5 and 9
    private val stated = "c1 :=> -holdsAt(f, 7).\n"

    // the times flow through r3: the instance of g(T) for 5 rests on the instance of holdsAt(f, T) for 5 alone
    private val derived = "r3 : holdsAt(f, T) => g(T).\nr4 : g(5) => ok5.\nr5 : g(7) => ok7.\n"

    // The label of each statement: in, out, und, or none when no argument concludes it
    private fun labels(
        theory: String,
        queryMode: Boolean,
        vararg statements: String,
    ): List<String> =
        statements.map { statement ->
            val solver = Arg2pSolverFactory.default(theory = theory + flags, settings = FlagsBuilder().queryMode(queryMode).create())
            val solution =
                if (queryMode) {
                    solver.solve(Struct.parse("answerQuery($statement, I, O, U)", solver.operators)).first()
                } else {
                    solver.solve(Struct.parse("buildLabelSets(I, O, U)", solver.operators)).first()
                }
            // buildLabelSets labels the statements, written [Claim]; answerQuery the claims
            val term = Struct.parse(if (queryMode) statement else "[$statement]", solver.operators)
            listOf("I" to "in", "O" to "out", "U" to "und")
                .firstOrNull { (name, _) ->
                    solution.substitution
                        .getByName(name)!!
                        .castToList()
                        .toList()
                        .any { it == term }
                }?.second ?: "none"
        }

    private fun bothModes(
        theory: String,
        statements: List<String>,
        expected: List<String>,
    ) = listOf(false, true).forEach { queryMode ->
        assertEquals(expected, labels(theory, queryMode, *statements.toTypedArray()), "queryMode $queryMode")
    }

    @Test
    fun anArgumentForASetSupportsItsValues() = bothModes(after, listOf("ok", "no"), listOf("in", "none"))

    // the premise c1 is preferred to the rule r0 (last link): the instance for 7 is out, the one for 5 stands
    @Test
    fun aConflictOnAValueDefeatsItsInstanceAlone() =
        bothModes(after + stated, listOf("ok", "holdsAt(f, 5)", "holdsAt(f, 7)"), listOf("in", "in", "out"))

    // -f over (5, 8]: the sets overlap there, so 6 is contested and 9 is not
    @Test
    fun setsConflictWhereTheyOverlap() {
        val overlapping = after + "c2 : {T > 5, T =< 8} => -holdsAt(f, T).\nr6 : holdsAt(f, 6) => ok6.\nr7 : holdsAt(f, 9) => ok9.\n"
        bothModes(overlapping, listOf("ok6", "ok9"), listOf("und", "in"))
    }

    @Test
    fun anInstanceRestsOnTheInstancesOfItsSubArguments() = bothModes(after + stated + derived, listOf("ok5", "ok7"), listOf("in", "out"))

    // without constraints too: p(X) holds for every X, and the attack on p(a) defeats what rests on p(a) alone
    @Test
    fun aFreeVariableIsASetOfValues() {
        val free = "f1 :=> q.\nr : q => p(X).\nc :=> -p(a).\ns : p(a) => t.\nu : p(b) => w.\n"
        bothModes(free, listOf("t", "w", "p(a)", "p(b)"), listOf("out", "in", "out", "in"))
    }

    @Test
    fun aValueOutsideASetIsNotAdmitted() {
        val members = "f1 :=> s.\nr : s, {member(X, [a, b])} => p(X).\nu1 : p(a) => ok1.\nu2 : p(c) => ok2.\n"
        bothModes(members, listOf("ok1", "ok2"), listOf("in", "none"))
    }

    // h(T) after 1 cannot support k(T) before 0: no argument for k, nor for what rests on it
    @Test
    fun constraintsThatCannotHoldTogetherMakeNoArgument() {
        val disjoint =
            "f1 :=> s.\nr0 : s, {T > 1} => h(T).\nr1 : h(T), {T < 0} => k(T).\nr2 : k(T) => z.\n" +
                "r3 : h(T), {T < 3} => m(T).\nr4 : m(T) => y.\n"
        bothModes(disjoint, listOf("z", "y"), listOf("none", "in"))
    }
}
