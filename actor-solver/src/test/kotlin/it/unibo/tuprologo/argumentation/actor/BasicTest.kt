package it.unibo.tuprologo.argumentation.actor

import akka.actor.typed.ActorSystem
import it.unibo.tuprolog.argumentation.actor.actors.KbDistributor
import it.unibo.tuprolog.argumentation.actor.actors.ResponseConsumer
import it.unibo.tuprolog.argumentation.actor.message.Add
import org.junit.Test
import kotlin.test.assertEquals

class BasicTest {

    @Test
    fun dummy() {
        val system = ActorSystem.create(KbDistributor.create(), "solver")
        system.tell(Add("r1 :=> b"))
        system.tell(Add("r2 : b => c"))

        ResponseConsumer.getResponse("c", system).also {
            assertEquals(it.inArgs.size, 1)
            assertEquals(it.outArgs.size, 0)
            assertEquals(it.undArgs.size, 0)
        }

        ResponseConsumer.getResponse("a", system).also {
            assertEquals(it.inArgs.size, 0)
            assertEquals(it.outArgs.size, 0)
            assertEquals(it.undArgs.size, 1)
        }

        ResponseConsumer.getResponse("b", system).also {
            assertEquals(it.inArgs.size, 1)
            assertEquals(it.outArgs.size, 0)
            assertEquals(it.undArgs.size, 0)
        }

        system.terminate()
    }

    @Test
    fun dummyDistribution() {
        val system = ActorSystem.create(KbDistributor.create(), "solver")
        system.tell(Add("r1 :=> b"))
        system.tell(Add("r3 : c => d"))
        system.tell(Add("r2 : b => c"))
        system.tell(Add("r4 : d => e"))
        system.tell(Add("r5 :=> undercut(r1)"))
        // system.tell(RequireEvaluation("c"))
        // system.tell(RequireEvaluation("b"))
        // system.tell(RequireEvaluation("undercut(r1)"))
        Thread.sleep(3000)
    }

    @Test
    fun dummyEvaluation() {
        val system = ActorSystem.create(KbDistributor.create(), "solver")
        system.tell(Add("r1 :=> b"))
        system.tell(Add("r5 :=> -b"))
        // system.tell(RequireEvaluation("b"))
        Thread.sleep(2000)
    }

    @Test
    fun dummyEvaluationWithNegation() {
        val system = ActorSystem.create(KbDistributor.create(), "solver")
        system.tell(Add("r5 :=> -b"))
        // system.tell(RequireEvaluation("-b"))
        Thread.sleep(2000)
    }
}
