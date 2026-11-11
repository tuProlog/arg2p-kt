package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.constraints

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

// Arguments holding for sets of values ({ } constraints) and the pieces the attacks split them into
// (graphExtension(constraints)), in the whole graph and in query mode
class ConstraintsLibraryTest {
    private val flags = "graphExtension(standardPref).\ngraphExtension(constraints).\n"

    // f holds after 1; a time after it is supported by its instance, a time before it by nothing
    private val after = "f1 :=> s.\nr0 : s, {T > 1} => holdsAt(f, T).\nr1 : holdsAt(f, 5) => ok.\nr2 : holdsAt(f, 0) => no.\n"

    // -f stated at 7: it attacks the argument for every time after 1, and its piece at 7, not those before and after
    private val stated = "c1 :=> -holdsAt(f, 7).\n"

    // the times flow through r3: g(5) rests on the piece of holdsAt(f, T) before 7, g(7) on the piece at 7
    private val derived = "r3 : holdsAt(f, T) => g(T).\nr4 : g(5) => ok5.\nr5 : g(7) => ok7.\n"

    // The label of each statement as solve/4 answers it, the graph built whole or for the query: in, out, und, or none
    // when no argument concludes it
    private fun labels(
        theory: String,
        queryMode: Boolean,
        vararg statements: String,
    ): List<String> =
        statements.map { statement ->
            val solver = Arg2pSolverFactory.default(theory = theory + flags, settings = Flags.defaults().queryMode(queryMode).library())
            val solution = solver.solve(Struct.parse("solve($statement, I, O, U)", solver.operators)).first()
            val term = Struct.parse(statement, solver.operators)
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

    // the premise c1 is preferred to the rule r0 (last link): the piece at 7 is out, the one before it stands
    @Test
    fun aConflictOnAValueDefeatsItsInstanceAlone() =
        bothModes(after + stated, listOf("ok", "holdsAt(f, 5)", "holdsAt(f, 7)"), listOf("in", "in", "out"))

    // -f over (5, 8]: the sets overlap there, so 6 is contested and 9 is not
    @Test
    fun setsConflictWhereTheyOverlap() {
        val overlapping = after + "c2 : {T > 5, T =< 8} => -holdsAt(f, T).\nr6 : holdsAt(f, 6) => ok6.\nr7 : holdsAt(f, 9) => ok9.\n"
        bothModes(overlapping, listOf("ok6", "ok9"), listOf("und", "in"))
    }

    // holdsAt(f, T) over (1, 4] stands, holdsAt(f, 6) is defeated: a statement and its instances are labelled each on
    // its own, whichever is labelled first
    @Test
    fun aStatementDoesNotLabelItsInstances() {
        val apart = "f1 :=> s.\nr0 : s, {T > 1, T =< 4} => holdsAt(f, T).\nr1 : s => holdsAt(f, 6).\nc1 :=> -holdsAt(f, 6).\n"
        bothModes(apart, listOf("holdsAt(f, 6)"), listOf("out"))
    }

    // 9 is a value no rule asks for: the piece of holdsAt(f, T) after 7 answers it, in both modes; 7 its piece at 7
    @Test
    fun aValueNothingUsesIsAnsweredByItsPiece() =
        bothModes(
            after + stated,
            listOf("holdsAt(f, 9)", "holdsAt(f, 3)", "holdsAt(f, 7)", "holdsAt(f, 0)"),
            listOf("in", "in", "out", "none"),
        )

    // g(T) has no attacker of its own: its pieces follow those of holdsAt(f, T), which it rests on
    @Test
    fun anArgumentIsSplitAsItsSubArguments() =
        bothModes(after + stated + derived, listOf("g(9)", "g(7)", "g(2)"), listOf("in", "out", "in"))

    // -f over (5, 8], stronger than r0 (a premise against a rule): the piece of holdsAt(f, T) inside it is out, the others in
    @Test
    fun anAttackOverARangeSplitsAtItsBounds() {
        val ranged = after + "c2 :=> s2.\nc3 : s2, {T > 5, T =< 8} -> -holdsAt(f, T).\n"
        bothModes(ranged, listOf("holdsAt(f, 5)", "holdsAt(f, 6)", "holdsAt(f, 8)", "holdsAt(f, 8.5)"), listOf("in", "out", "out", "in"))
    }

    // an attacker binding X to a term: the piece of p(X) other than a stands, whatever X is
    @Test
    fun aBindingSplitsByDifference() {
        val free = "f1 :=> q.\nr : q => p(X).\nc :=> -p(a).\n"
        bothModes(free, listOf("p(a)", "p(b)", "p(f(c))"), listOf("out", "in", "in"))
    }

    // a linear relation between the two variables of p: the pieces are on either side of the line X + Y = 3
    @Test
    fun anAttackSplitsAlongALinearRelation() {
        val plane = "f1 :=> s.\nr : s, {X > 0, Y > 0} => p(X, Y).\nc0 :=> s2.\nc : s2, {X + Y < 3} -> -p(X, Y).\n"
        bothModes(plane, listOf("p(1, 1)", "p(2, 0.5)", "p(2, 2)", "p(1, 2)"), listOf("out", "out", "in", "in"))
    }

    // the attacker ranges over a time X of its own (after a start between 6 and 8): its region on holdsAt(f, T) needs
    // X, so it cuts nothing and the attack holds for every value
    @Test
    fun anAttackNeedingVariablesOfItsOwnHoldsForAllValues() {
        val unbounded =
            after + "f2 :=> e.\ne0 : e, {X > 6, X < 8} => started(X).\nc : started(X), {S > X} -> -holdsAt(f, S).\nsup(e0, r0).\n"
        bothModes(unbounded, listOf("holdsAt(f, 3)", "holdsAt(f, 9)"), listOf("out", "out"))
    }

    // two open arguments rebutting each other, -f over (5, 8] the stronger: each is split by the other, until neither
    // splits further
    @Test
    fun openArgumentsSplitEachOther() {
        val mutual = after + "c2 :=> s2.\nc3 : s2, {T > 5, T =< 8} => -holdsAt(f, T).\nsup(c3, r0).\n"
        bothModes(mutual, listOf("holdsAt(f, 3)", "holdsAt(f, 6)", "-holdsAt(f, 6)", "holdsAt(f, 9)"), listOf("in", "out", "in", "in"))
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
