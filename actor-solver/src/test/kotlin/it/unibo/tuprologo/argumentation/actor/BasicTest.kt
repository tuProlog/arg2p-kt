package it.unibo.tuprologo.argumentation.actor

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import it.unibo.tuprolog.argumentation.actor.ClusterInitializer
import it.unibo.tuprolog.argumentation.actor.actors.ResponseConsumer
import it.unibo.tuprolog.argumentation.actor.message.Add
import it.unibo.tuprolog.argumentation.actor.message.KbMessage
import it.unibo.tuprolog.argumentation.actor.message.Reset
import it.unibo.tuprolog.argumentation.actor.parallel
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.libs.basic.FlagsBuilder
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.classic.ClassicSolverFactory
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import org.junit.Ignore
import org.junit.Test
import kotlin.test.assertEquals

val cluster = ClusterInitializer.joinCluster("127.0.0.1:2552", "2552")

private fun withActorSystem(execute: (ActorSystem<KbMessage>, ActorRef<KbMessage>) -> Unit) =
    cluster.let {
        it.second.tell(Reset)
        execute(it.first, it.second)
    }

class BasicTest {

    @Test
    fun dummy() {
        withActorSystem { system, master ->
            master.tell(Add("r1 :=> b"))
            master.tell(Add("r2 : b => c"))

            ResponseConsumer.getResponse("c", system, master).also {
                assertEquals(it.inArgs.size, 1)
                assertEquals(it.outArgs.size, 0)
                assertEquals(it.undArgs.size, 0)
            }

            ResponseConsumer.getResponse("a", system, master).also {
                assertEquals(it.inArgs.size, 0)
                assertEquals(it.outArgs.size, 0)
                assertEquals(it.undArgs.size, 1)
            }

            ResponseConsumer.getResponse("b", system, master).also {
                assertEquals(it.inArgs.size, 1)
                assertEquals(it.outArgs.size, 0)
                assertEquals(it.undArgs.size, 0)
            }
        }
    }

    @Test
    fun dummyDistribution() {
        withActorSystem { system, master ->
            master.tell(Add("r1 :=> b"))
            master.tell(Add("r3 : c => d"))
            master.tell(Add("r2 : b => c"))
            master.tell(Add("r4 : d => e"))
            master.tell(Add("r5 :=> undercut(r1)"))

            ResponseConsumer.getResponse("c", system, master).also {
                assertEquals(it.inArgs.size, 0)
                assertEquals(it.outArgs.size, 1)
                assertEquals(it.undArgs.size, 0)
            }

            ResponseConsumer.getResponse("b", system, master).also {
                assertEquals(it.inArgs.size, 0)
                assertEquals(it.outArgs.size, 1)
                assertEquals(it.undArgs.size, 0)
            }

            ResponseConsumer.getResponse("undercut(r1)", system, master).also {
                assertEquals(it.inArgs.size, 1)
                assertEquals(it.outArgs.size, 0)
                assertEquals(it.undArgs.size, 0)
            }
        }
    }

    @Test
    fun dummyEvaluation() {
        withActorSystem { system, master ->
            master.tell(Add("r1 :=> b"))
            master.tell(Add("r5 :=> -b"))

            ResponseConsumer.getResponse("b", system, master).also {
                assertEquals(it.inArgs.size, 0)
                assertEquals(it.outArgs.size, 0)
                assertEquals(it.undArgs.size, 1)
            }
        }
    }

    @Test
    fun dummyEvaluationWithNegation() {
        withActorSystem { system, master ->
            master.tell(Add("r1 :=> -b"))

            ResponseConsumer.getResponse("-b", system, master).also {
                assertEquals(it.inArgs.size, 1)
                assertEquals(it.outArgs.size, 0)
                assertEquals(it.undArgs.size, 0)
            }
        }
    }

    @Test
    fun stressTest() {
        val rules = IntRange(1, 10).joinToString("\n") {
            listOf(
                "r0$it :=> a$it.",
                "r1$it : a$it => b$it.",
                "r2$it : b$it => c$it.",
                "r3$it : c$it => d$it."
            ).joinToString("\n")
        }

        arg2pScope {
            ClassicSolverFactory.mutableSolverWithDefaultBuiltins(
                otherLibraries = Arg2pSolver.parallel().to2pLibraries(),
                flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL)
            ).also {
                it.loadStaticKb(Theory.parse(rules, it.operators))
                it.solve("join"(2551) and "load").first()
                val a = it.solve("solve"("d9", X, Y, Z)).first()
                assertEquals(a.substitution[X].toString(), "[d9]")
                it.solve(Struct.parse("leave")).first()
            }
        }
    }

    @Ignore
    @Test
    fun stressTestIterative() {
        val rules = IntRange(1, 100).joinToString("\n") {
            listOf(
                "r0$it :=> a$it.",
                "r1$it : a$it => b$it.",
                "r2$it : b$it => c$it.",
                "r3$it : c$it => d$it."
            ).joinToString("\n")
        }

        arg2pScope {
            ClassicSolverFactory.mutableSolverWithDefaultBuiltins(
                otherLibraries = Arg2pSolver.default(
                    kotlin.collections.listOf(FlagsBuilder().create())
                ).to2pLibraries(),
                flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL)
            ).also {
                it.loadStaticKb(Theory.parse(rules, it.operators))
                val a = it.solve("answerQuery"("d99", X, Y, Z)).first()
                assertEquals(a.isYes, true)
            }
        }
    }
}
