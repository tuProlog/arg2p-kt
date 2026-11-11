package it.unibo.tuprolog.argumentation.core.libraries.framework.store

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/** An argument is kept once in the context: looked up by the claim inside it, by its identifier through `arg/1` */
class ArgumentsInContextTest {
    private fun holds(goal: String): Boolean {
        val solver = solverWithTheory("")
        return solver.solve(Struct.parse("contextReset, $goal", solver.operators)).first().isYes
    }

    private val facts =
        (1..12).map { "contextAssert(argument([r, t, [p($it)], b, i]))" } +
            listOf(
                "contextAssert(argument([r, t, [p(X)], b, i]))",
                "contextAssert(argument([r, t, [q], b, i]))",
                "contextAssert(argument([r, t, [Y], b, i]))",
                "contextAssert(argument([r, t | T]))",
                "contextAssert(argument(foo))",
                "contextAssert(argument([r]))",
            )

    private fun answers(
        goal: String,
        before: String = "true",
    ): Int {
        val solver = solverWithTheory("")
        val all = (listOf("contextReset") + facts + before).joinToString(", ")
        val solution = solver.solve(Struct.parse("$all, findall(x, contextCheck($goal), L), length(L, N)", solver.operators)).first()
        assertTrue(solution.isYes, goal)
        return solution.substitution.entries
            .first { it.key.name == "N" }
            .value
            .castToInteger()
            .value
            .toInt()
    }

    // (each count written by hand, the index not being what tells them: a claim, the variable one, the open one)
    @Test
    fun lookupsByTheClaimFindWhatUnifies() {
        assertEquals(4, answers("argument([_, _, [p(3)], _, _])"))
        assertEquals(3, answers("argument([_, _, [q], _, _])"))
        assertEquals(2, answers("argument([_, _, [z], _, _])"))
        assertEquals(15, answers("argument([_, _, [p(Q)], _, _])"))
        assertEquals(18, answers("argument(Q)"))
        assertEquals(1, answers("argument(foo)"))
        assertEquals(0, answers("argument(bar)"))
    }

    @Test
    fun lookupsByTheClaimAfterARemoval() {
        val before = "contextCheck(argument([_, _, [p(5)], _, _])), contextRetract(argument([_, _, [p(3)], _, _]))"
        assertEquals(1, answers("argument([_, _, [p(4)], _, _])", before))
        assertEquals(0, answers("argument([_, _, [p(3)], _, _])", before))
        assertEquals(14, answers("argument(Q)", before))
    }

    @Test
    fun theTermOfAnIdentifier() {
        assertTrue(holds("contextId(k(X, a), I), contextTerm(I, k(Q, R)), var(Q), R == a"))
        assertTrue(holds("\\+ contextTerm(9999, _), \\+ contextTerm(a, _)"))
    }

    @Test
    fun theClauseOfAnIdentifierAdded() {
        assertTrue(holds("contextAbsent(k(1)), contextId(k(1), I), contextAbsent(k(1)), contextAssertId(I), contextCheck(k(1))"))
        assertTrue(holds("\\+ contextAssertId(9999)"))
    }

    @Test
    fun lookupsByAKey() {
        val saved =
            "contextAssert(argument([r, t, [p(a)], b, i])), contextAssert(argument([s, t, [p(Z)], b, i])), " +
                "contextAssert(argument([u, t, [q], b, i]))"
        assertTrue(holds("$saved, findall(R, contextBy(conclusion, [p(a)], argument([R | _])), [s, r])"))
        assertTrue(holds("$saved, findall(C, contextBy(conclusion, C, argument(_)), [[q], [p(_)], [p(a)]])"))
        assertTrue(holds("$saved, contextBy(conclusion, [q], argument(A)), A == [u, t, [q], b, i]"))
        assertTrue(holds("$saved, \\+ contextBy(conclusion, [r], argument(_))"))
        assertTrue(holds("$saved, \\+ contextBy(unknown, [q], argument(_))"))
    }

    @Test
    fun factsMatchingAsStored() {
        val saved =
            "contextAssert(argument([s, t, [p(Z)], b, i])), contextAssert(argument([r, t, [p(a)], b, i])), " +
                "contextAssert(k2([r, t, [q], b, i]) :- z)"
        assertTrue(holds("$saved, contextMatching(conclusion, [p(1)], argument(A)), A = [s, _, [p(V)], _, _], var(V)"))
        assertTrue(holds("$saved, K = [p(Y)], findall(R, contextMatching(conclusion, K, argument([R | _])), [r, s]), var(Y)"))
        assertTrue(holds("$saved, \\+ contextMatching(conclusion, [q], k2(_))"))
    }

    private val argument = "[[r], r, [p(a)], [], [[], [], []]]"
    private val other = "[[s, r], s, [q(a)], [p(a)], [[s], [s, r], []]]"

    @Test
    fun aLabelSharesTheRecordOfItsArgument() {
        assertTrue(
            holds(
                """
                A = $argument,
                store::saveArgument([p(a)], A),
                store::argumentId(A, I),
                store::labelArgument(in, A, I),
                contextCheck(in(B)), B == A,
                contextCheck(argument(C)), C == A,
                \+ contextCheck(out(_)),
                contextActive(Branch),
                contextBranch(Branch, _),
                store::unlabelArguments(in),
                \+ contextCheck(in(_)),
                contextCheck(Branch, in(D)), D == A,
                contextCheck(argument(_))
                """.trimIndent(),
            ),
        )
    }

    @Test
    fun theRelationsOfTheGraphAreAnsweredFromTheIdentifiers() {
        assertTrue(
            holds(
                """
                A = $argument, B = $other,
                store::saveArgument([p(a)], A),
                store::saveArgument([q(a)], B),
                store::saveSupport(A, B),
                store::saveAttack(rebut, A, B, B),
                contextCheck(support(A, B)),
                contextCheck(support(X, B)), X == A,
                contextCheck(support(A, Y)), Y == B,
                \+ contextCheck(support(B, A)),
                contextCheck(attack(rebut, A, B, B)),
                contextCheck(attack(T, _, _, _)), T == rebut,
                \+ contextCheck(attack(undercut, _, _, _)),
                contextActive(Branch),
                contextBranch(Branch, _),
                store::removeAttack(rebut, A, B, B),
                \+ contextCheck(attack(_, _, _, _)),
                contextCheck(Branch, attack(rebut, A, B, B)),
                contextCheck(Branch, support(A, B))
                """.trimIndent(),
            ),
        )
    }

    @Test
    fun aRelationAssertedAsClausesIsLookedUpAsClauses() {
        assertTrue(holds("contextAssert(support(x, y)), contextCheck(support(x, Y)), Y == y, \\+ contextCheck(support(y, _))"))
    }

    @Test
    fun theInfoOfAnArgumentByItsIdentifier() {
        assertTrue(
            holds(
                "A = $argument, store::saveArgument([p(a)], A), store::argumentId(A, I), " +
                    "store::argumentInfoId(I, K), store::infoById(K, [[], [], []]), contextTerm(K, info([[], [], []]))",
            ),
        )
    }

    @Test
    fun aStatementLabelIsKeptOnce() {
        assertTrue(
            holds(
                "store::labelStatement(in, [p(X)]), store::statementLabel(in, [p(a)]), " +
                    "store::statementLabelled(in, [p(Y)]), \\+ store::statementLabelled(in, [p(a)]), contextCheck(statIn([p(_)]))",
            ),
        )
    }

    @Test
    fun theIdentifierOfAStoredFact() {
        assertTrue(
            holds(
                "A = $argument, store::saveArgument([p(a)], A), store::argumentId(A, I), " +
                    "contextStoredId(argument(_), J), J == I, \\+ contextStoredId(argument(foo), _), contextAssert(k(1)), \\+ contextStoredId(k(_), _)",
            ),
        )
    }

    @Test
    fun savedOnceAndFoundByItsConclusionAndIdentifier() {
        assertTrue(
            holds(
                """
                A = $argument,
                store::saveArgument([p(a)], A),
                \+ store::saveArgument([p(a)], A),
                store::argumentFor([p(a)], B), B == A,
                store::argumentId(A, I),
                store::argumentById(I, C), C == A,
                store::argumentById(J, _), J == I,
                store::argumentIds([I])
                """.trimIndent(),
            ),
        )
    }

    @Test
    fun theConclusionIsThatOfTheArgument() {
        assertTrue(holds("A = $argument, \\+ store::saveArgument([p(b)], A), store::argumentIds([])"))
    }

    @Test
    fun excludedArgumentsAreStillSaved() {
        assertTrue(
            holds(
                """
                A = $argument,
                store::saveArgument([p(a)], A),
                store::argumentId(A, I),
                contextActive(Branch),
                contextBranch(Branch, _),
                store::leaveOutArguments,
                \+ store::argumentById(I, _),
                store::argumentIds([]),
                store::argument(A),
                store::argumentFor([p(a)], _),
                \+ store::saveArgument([p(a)], A),
                store::inGraph(Branch, I), contextTerm(I, argument(B)), B == A,
                store::takeInArgument(I),
                store::argumentById(I, _)
                """.trimIndent(),
            ),
        )
    }

    @Test
    fun anInstanceOfASavedArgument() {
        assertTrue(
            holds(
                """
                store::saveArgument([p(V)], [[r], r, [p(V)], [], [[], [], []]]),
                store::argumentUnifiable([p(a)], General),
                General = [_, _, [p(W)], _, _], var(W),
                Instance = $argument,
                \+ store::saveArgument([p(a)], Instance),
                store::saveInstance([p(a)], Instance),
                store::argumentId(Instance, I),
                store::argumentById(I, Saved), Saved == Instance
                """.trimIndent(),
            ),
        )
    }
}
