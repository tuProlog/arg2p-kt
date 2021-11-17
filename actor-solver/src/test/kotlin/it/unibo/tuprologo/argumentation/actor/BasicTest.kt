package it.unibo.tuprologo.argumentation.actor

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import it.unibo.tuprolog.argumentation.actor.ClusterInitializer
import it.unibo.tuprolog.argumentation.actor.actors.ResponseConsumer
import it.unibo.tuprolog.argumentation.actor.message.Add
import it.unibo.tuprolog.argumentation.actor.message.KbMessage
import it.unibo.tuprolog.argumentation.actor.message.Reset
import org.junit.Test
import kotlin.test.assertEquals

val cluster = ClusterInitializer.joinCluster("127.0.0.1:2551", "2551")

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
}
