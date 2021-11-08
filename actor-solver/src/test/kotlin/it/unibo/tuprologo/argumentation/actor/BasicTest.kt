package it.unibo.tuprologo.argumentation.actor

import akka.actor.typed.ActorSystem
import it.unibo.tuprolog.argumentation.actor.Add
import it.unibo.tuprolog.argumentation.actor.Eval
import it.unibo.tuprolog.argumentation.actor.KbDistributor
import org.junit.Test

class BasicTest {

    @Test
    fun dummy() {
        val system = ActorSystem.create(KbDistributor.create(), "solver")
        system.tell(Add("r1 :=> b"))
        system.tell(Add("r2 : b => c"))
        system.tell(Eval("c"))
        system.tell(Eval("a"))
        system.tell(Eval("b"))
        Thread.sleep(2000)
    }

    @Test
    fun dummyDistribution() {
        val system = ActorSystem.create(KbDistributor.create(), "solver")
        system.tell(Add("r1 :=> b"))
        system.tell(Add("r3 : c => d"))
        system.tell(Add("r2 : b => c"))
        system.tell(Add("r4 : d => e"))
        system.tell(Eval("c"))
        system.tell(Eval("a"))
        system.tell(Eval("b"))
        Thread.sleep(2000)
    }
}
