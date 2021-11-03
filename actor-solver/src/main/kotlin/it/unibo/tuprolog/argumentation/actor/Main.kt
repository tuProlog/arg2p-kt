package it.unibo.tuprolog.argumentation.actor

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.javadsl.AbstractBehavior
import akka.actor.typed.javadsl.ActorContext
import akka.actor.typed.javadsl.Behaviors
import akka.actor.typed.javadsl.Receive
import it.unibo.tuprolog.argumentation.actor.HelloWorld.Greet
import it.unibo.tuprolog.argumentation.actor.HelloWorld.Greeted

fun main () {
    val system = ActorSystem.create(HelloWorldMain.create(), "hello")
    system.tell(HelloWorldMain.SayHello("World"))
}

class HelloWorld private constructor(context: ActorContext<Greet>) : AbstractBehavior<Greet>(context) {

    class Greet(val whom: String, val replyTo: ActorRef<Greeted>)
    class Greeted(val whom: String, val from: ActorRef<Greet>)

    override fun createReceive(): Receive<Greet> {
        return newReceiveBuilder().onMessage(
            Greet::class.java
        ) { command: Greet ->
            onGreet(
                command
            )
        }.build()
    }

    private fun onGreet(command: Greet): Behavior<Greet?> {
        context.log.info("Hello {}!", command.whom)
        command.replyTo.tell(Greeted(command.whom, context.self))
        return this
    }

    companion object {
        fun create(): Behavior<Greet> {
            return Behaviors.setup { context: ActorContext<Greet> ->
                HelloWorld(
                    context
                )
            }
        }
    }
}

class HelloWorldBot private constructor(context: ActorContext<Greeted>, private val max: Int) : AbstractBehavior<Greeted>(context) {
    private var greetingCounter = 0
    override fun createReceive(): Receive<Greeted> {
        return newReceiveBuilder().onMessage(
            Greeted::class.java
        ) { message: Greeted -> onGreeted(message) }.build()
    }

    private fun onGreeted(message: Greeted): Behavior<Greeted?> {
        greetingCounter++
        context.log.info("Greeting {} for {}", greetingCounter, message.whom)
        return if (greetingCounter == max) {
            Behaviors.stopped()
        } else {
            message.from.tell(Greet(message.whom, context.self))
            this
        }
    }

    companion object {
        fun create(max: Int): Behavior<Greeted> {
            return Behaviors.setup { context: ActorContext<Greeted> ->
                HelloWorldBot(
                    context,
                    max
                )
            }
        }
    }
}

class HelloWorldMain private constructor(context: ActorContext<SayHello>) : AbstractBehavior<HelloWorldMain.SayHello>(context) {

    class SayHello(val name: String)

    private val greeter: ActorRef<Greet>

    override fun createReceive(): Receive<SayHello> {
        return newReceiveBuilder().onMessage(
            SayHello::class.java
        ) { command: SayHello ->
            onStart(
                command
            )
        }.build()
    }

    private fun onStart(command: SayHello): Behavior<SayHello?> {
        val replyTo = context.spawn(HelloWorldBot.create(3), command.name)
        greeter.tell(Greet(command.name, replyTo))
        return this
    }

    companion object {
        fun create(): Behavior<SayHello> {
            return Behaviors.setup { context: ActorContext<SayHello> ->
                HelloWorldMain(
                    context
                )
            }
        }
    }

    init {
        greeter = context.spawn(HelloWorld.create(), "greeter")
    }
}
