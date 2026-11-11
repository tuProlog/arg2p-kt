package it.unibo.tuprologo.argumentation.actor

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.actor.libraries.actorGraph.ActorGraph
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

// The graph explored one argument at a time (buildArgument/3, findAttacker/6), as the actors do
class ActorGraphTest {
    // c from the premise b, which -b undermines; r3 undercuts the rule of c, r5 the premise (not an attack)
    private val theory = "r1 :=> b.\nr2 : b => c.\nr3 :=> undercut(r2).\nr4 :=> -b.\nr5 :=> undercut(r1)."

    // A solver of [theory] with the module, its rules converted
    private class Module(
        theory: String,
    ) {
        private val solver =
            Arg2pSolverFactory.default(theory, Flags.defaults().library(), dynamicLibs = listOf(ActorGraph)).also {
                it.solve(Struct.parse("contextReset", it.operators)).first()
            }

        fun holds(goal: String): Boolean = solver.solve(Struct.parse(goal, solver.operators)).first().isYes

        fun values(
            goal: String,
            variable: String,
        ): List<String> =
            solver
                .solve(Struct.parse(goal, solver.operators))
                .filter { it.isYes }
                .map { it.substitution.getByName(variable).toString() }
                .toList()

        fun value(
            goal: String,
            variable: String,
        ): String? = values(goal, variable).firstOrNull()
    }

    private fun module(theory: String = this.theory) = Module(theory)

    @Test
    fun theArgumentsForAQuery() {
        val module = module()
        assertEquals(
            listOf("[[r2, r1], r2, [c], [b], [[r2], [r2], [r1]]]"),
            module.values("actorGraph::buildArgument(c, A, _)", "A"),
        )
        assertTrue(module.holds("actorGraph::buildArgument(c, A, _), contextCheck(argument(A))"))
        assertEquals(listOf("b"), module.values("actorGraph::buildArgument(c, _, S), member(A, S), store::claim(A, C)", "C"))
    }

    @Test
    fun theAttackersOfAnArgumentAndOfItsSubArguments() {
        val module = module()
        val target = module.value("actorGraph::buildArgument(c, A, _)", "A")!!
        val subArguments = module.value("actorGraph::buildArgument(c, _, S)", "S")!!
        val attackers = module.values("actorGraph::findAttacker($target, $subArguments, [], B, _, yes), store::claim(B, C)", "C")
        // the undercut of the rule of c, and the undermining of b: a premise is not undercut
        assertEquals(setOf("undercut(r2)", "'-'(b)"), attackers.toSet())
        assertEquals(2, attackers.size)
        assertTrue(module.holds("store::attack(undermine, B, $target, _), store::claim(B, '-'(b))"))
    }

    @Test
    fun theAttackersOfAnArgumentEvaluatedAgain() {
        val module = module()
        val target = module.value("actorGraph::buildArgument(c, A, _)", "A")!!
        val subArguments = module.value("actorGraph::buildArgument(c, _, S)", "S")!!
        val query = "actorGraph::findAttacker($target, $subArguments, [], B, _, yes), store::claim(B, C)"
        assertEquals(module.values(query, "C"), module.values(query, "C"))
    }

    @Test
    fun theAttackersOfAnArgumentBuiltElsewhere() {
        val builder = module()
        val target = builder.value("actorGraph::buildArgument(c, A, _)", "A")!!
        val subArguments = builder.value("actorGraph::buildArgument(c, _, S)", "S")!!
        // a solver without the rules of the argument
        val module = module("r4 :=> -b.\nr5 :=> undercut(r1).")
        val attackers = module.values("actorGraph::findAttacker($target, $subArguments, [], B, _, yes), store::claim(B, C)", "C")
        assertEquals(listOf("'-'(b)"), attackers)
    }

    @Test
    fun anAttackerInTheQueryChainIsACycle() {
        val module = module()
        val target = module.value("actorGraph::buildArgument(c, A, _)", "A")!!
        val subArguments = module.value("actorGraph::buildArgument(c, _, S)", "S")!!
        val undercut = module.value("actorGraph::buildArgument(undercut(r2), A, _)", "A")!!
        assertTrue(module.holds("actorGraph::findAttacker($target, $subArguments, [$undercut], $undercut, [], no)"))
    }
}
