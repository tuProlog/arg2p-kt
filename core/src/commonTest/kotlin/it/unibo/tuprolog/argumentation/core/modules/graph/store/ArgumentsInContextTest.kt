package it.unibo.tuprolog.argumentation.core.modules.graph.store

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
        return solver.solve(Struct.parse("context_reset, $goal", solver.operators)).first().isYes
    }

    private val facts =
        (1..12).map { "context_assert(argument([r, t, [p($it)], b, i]))" } +
            listOf(
                "context_assert(argument([r, t, [p(X)], b, i]))",
                "context_assert(argument([r, t, [q], b, i]))",
                "context_assert(argument([r, t, [Y], b, i]))",
                "context_assert(argument([r, t | T]))",
                "context_assert(argument(foo))",
                "context_assert(argument([r]))",
            )

    private fun answers(
        goal: String,
        before: String = "true",
    ): Int {
        val solver = solverWithTheory("")
        val all = (listOf("context_reset") + facts + before).joinToString(", ")
        val solution = solver.solve(Struct.parse("$all, findall(x, context_check($goal), L), length(L, N)", solver.operators)).first()
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
        val before = "context_check(argument([_, _, [p(5)], _, _])), context_retract(argument([_, _, [p(3)], _, _]))"
        assertEquals(1, answers("argument([_, _, [p(4)], _, _])", before))
        assertEquals(0, answers("argument([_, _, [p(3)], _, _])", before))
        assertEquals(14, answers("argument(Q)", before))
    }

    @Test
    fun theTermOfAnIdentifier() {
        assertTrue(holds("context_id(k(X, a), I), context_term(I, k(Q, R)), var(Q), R == a"))
        assertTrue(holds("\\+ context_term(9999, _), \\+ context_term(a, _)"))
    }

    @Test
    fun theClauseOfAnIdentifierAdded() {
        assertTrue(holds("context_absent(k(1)), context_id(k(1), I), context_absent(k(1)), context_assert_id(I), context_check(k(1))"))
        assertTrue(holds("\\+ context_assert_id(9999)"))
    }

    @Test
    fun lookupsByAKey() {
        val saved =
            "context_assert(argument([r, t, [p(a)], b, i])), context_assert(argument([s, t, [p(Z)], b, i])), " +
                "context_assert(argument([u, t, [q], b, i]))"
        assertTrue(holds("$saved, findall(R, context_by(conclusion, [p(a)], argument([R | _])), [s, r])"))
        assertTrue(holds("$saved, findall(C, context_by(conclusion, C, argument(_)), [[q], [p(_)], [p(a)]])"))
        assertTrue(holds("$saved, context_by(conclusion, [q], argument(A)), A == [u, t, [q], b, i]"))
        assertTrue(holds("$saved, \\+ context_by(conclusion, [r], argument(_))"))
        assertTrue(holds("$saved, \\+ context_by(unknown, [q], argument(_))"))
    }

    @Test
    fun factsMatchingAsStored() {
        val saved =
            "context_assert(argument([s, t, [p(Z)], b, i])), context_assert(argument([r, t, [p(a)], b, i])), " +
                "context_assert(k2([r, t, [q], b, i]) :- z)"
        assertTrue(holds("$saved, context_matching(conclusion, [p(1)], argument(A)), A = [s, _, [p(V)], _, _], var(V)"))
        assertTrue(holds("$saved, K = [p(Y)], findall(R, context_matching(conclusion, K, argument([R | _])), [r, s]), var(Y)"))
        assertTrue(holds("$saved, \\+ context_matching(conclusion, [q], k2(_))"))
    }

    private val argument = "[[r], r, [p(a)], [], [[], [], []]]"
    private val other = "[[s, r], s, [q(a)], [p(a)], [[s], [s, r], []]]"

    @Test
    fun aLabelSharesTheRecordOfItsArgument() {
        assertTrue(
            holds(
                """
                A = $argument,
                graphStore::saveArgument([p(a)], A),
                graphStore::argumentId(A, I),
                graphStore::labelArgument(in, A, I),
                context_check(in(B)), B == A,
                context_check(argument(C)), C == A,
                \+ context_check(out(_)),
                context_active(Branch),
                context_branch(Branch, _),
                graphStore::unlabelArguments(in),
                \+ context_check(in(_)),
                context_check(Branch, in(D)), D == A,
                context_check(argument(_))
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
                graphStore::saveArgument([p(a)], A),
                graphStore::saveArgument([q(a)], B),
                graphStore::saveSupport(A, B),
                graphStore::saveAttack(rebut, A, B, B),
                context_check(support(A, B)),
                context_check(support(X, B)), X == A,
                context_check(support(A, Y)), Y == B,
                \+ context_check(support(B, A)),
                context_check(attack(rebut, A, B, B)),
                context_check(attack(T, _, _, _)), T == rebut,
                \+ context_check(attack(undercut, _, _, _)),
                context_active(Branch),
                context_branch(Branch, _),
                graphStore::removeAttack(rebut, A, B, B),
                \+ context_check(attack(_, _, _, _)),
                context_check(Branch, attack(rebut, A, B, B)),
                context_check(Branch, support(A, B))
                """.trimIndent(),
            ),
        )
    }

    @Test
    fun aRelationAssertedAsClausesIsLookedUpAsClauses() {
        assertTrue(holds("context_assert(support(x, y)), context_check(support(x, Y)), Y == y, \\+ context_check(support(y, _))"))
    }

    @Test
    fun theInfoOfAnArgumentByItsIdentifier() {
        assertTrue(
            holds(
                "A = $argument, graphStore::saveArgument([p(a)], A), graphStore::argumentId(A, I), " +
                    "graphStore::argumentInfoId(I, K), graphStore::infoById(K, [[], [], []]), context_term(K, info([[], [], []]))",
            ),
        )
    }

    @Test
    fun aStatementLabelIsKeptOnce() {
        assertTrue(
            holds(
                "graphStore::labelStatement(in, [p(X)]), graphStore::statementLabel(in, [p(a)]), " +
                    "graphStore::statementLabelled(in, [p(Y)]), \\+ graphStore::statementLabelled(in, [p(a)]), context_check(statIn([p(_)]))",
            ),
        )
    }

    @Test
    fun theIdentifierOfAStoredFact() {
        assertTrue(
            holds(
                "A = $argument, graphStore::saveArgument([p(a)], A), graphStore::argumentId(A, I), " +
                    "context_stored_id(argument(_), J), J == I, \\+ context_stored_id(argument(foo), _), context_assert(k(1)), \\+ context_stored_id(k(_), _)",
            ),
        )
    }

    @Test
    fun savedOnceAndFoundByItsConclusionAndIdentifier() {
        assertTrue(
            holds(
                """
                A = $argument,
                graphStore::saveArgument([p(a)], A),
                \+ graphStore::saveArgument([p(a)], A),
                graphStore::argumentFor([p(a)], B), B == A,
                graphStore::argumentId(A, I),
                graphStore::argumentById(I, C), C == A,
                graphStore::argumentById(J, _), J == I,
                graphStore::argumentIds([I])
                """.trimIndent(),
            ),
        )
    }

    @Test
    fun theConclusionIsThatOfTheArgument() {
        assertTrue(holds("A = $argument, \\+ graphStore::saveArgument([p(b)], A), graphStore::argumentIds([])"))
    }

    @Test
    fun excludedArgumentsAreStillSaved() {
        assertTrue(
            holds(
                """
                A = $argument,
                graphStore::saveArgument([p(a)], A),
                graphStore::argumentId(A, I),
                context_active(Branch),
                context_branch(Branch, _),
                graphStore::excludeArguments,
                \+ graphStore::argumentById(I, _),
                graphStore::argumentIds([]),
                graphStore::argument(A),
                graphStore::argumentFor([p(a)], _),
                \+ graphStore::saveArgument([p(a)], A),
                graphStore::savedIdIn(Branch, I), context_term(I, argument(B)), B == A,
                graphStore::includeArgument(I),
                graphStore::argumentById(I, _)
                """.trimIndent(),
            ),
        )
    }

    @Test
    fun anInstanceOfASavedArgument() {
        assertTrue(
            holds(
                """
                graphStore::saveArgument([p(V)], [[r], r, [p(V)], [], [[], [], []]]),
                graphStore::argumentUnifiable([p(a)], General),
                General = [_, _, [p(W)], _, _], var(W),
                Instance = $argument,
                \+ graphStore::saveArgument([p(a)], Instance),
                graphStore::saveInstance([p(a)], Instance),
                graphStore::argumentId(Instance, I),
                graphStore::argumentById(I, Saved), Saved == Instance
                """.trimIndent(),
            ),
        )
    }
}
