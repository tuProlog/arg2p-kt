package it.unibo.tuprolog.argumentation.actor

import akka.actor.AddressFromURIString
import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.javadsl.Behaviors
import akka.cluster.Cluster
import akka.cluster.sharding.typed.javadsl.ClusterSharding
import akka.cluster.sharding.typed.javadsl.Entity
import akka.cluster.sharding.typed.javadsl.EntityTypeKey
import akka.cluster.typed.ClusterSingleton
import akka.cluster.typed.SingletonActor
import com.typesafe.config.ConfigFactory
import it.unibo.tuprolog.argumentation.actor.actors.Evaluator
import it.unibo.tuprolog.argumentation.actor.actors.KbDistributor
import it.unibo.tuprolog.argumentation.actor.message.KbMessage

object ClusterInitializer {
    private fun createConfiguration(port: String) =
        ConfigFactory.load(
            ConfigFactory
                .parseString(
                    """
         akka {
            actor {
                provider = "cluster"
                serializers {
                    jackson-cbor = "akka.serialization.jackson.JacksonCborSerializer"
                }
            
                serialization-bindings {
                  "it.unibo.tuprolog.argumentation.actor.message.KbMessage" = jackson-cbor
                }
            }
            remote.artery {
                canonical {
                    hostname = "127.0.0.1"
                    port = $port
                }
            }
            cluster {
                downing-provider-class = "akka.cluster.sbr.SplitBrainResolverProvider"
            }
            serialization.jackson {
              jackson-modules += "com.fasterxml.jackson.module.kotlin.KotlinModule"
            }
        }
        """,
                ).withFallback(ConfigFactory.load()),
        )

    fun joinCluster(
        seedAddress: String,
        localPort: String,
    ): Pair<ActorSystem<KbMessage>, ActorRef<KbMessage>> {
        val actorSystem: ActorSystem<KbMessage> =
            ActorSystem.create(Behaviors.empty(), "actor_solver", createConfiguration(localPort))

        Cluster.get(actorSystem).join(AddressFromURIString.parse("akka://actor_solver@${seedAddress.replace("'", "")}"))

        val masterActor =
            ClusterSingleton
                .get(actorSystem)
                .init(SingletonActor.of(KbDistributor.create(), "distributor"))

        ClusterSharding.get(actorSystem).init(
            Entity.of(EntityTypeKey.create(KbMessage::class.java, "evaluator")) { ctx ->
                Evaluator.create(masterActor, ctx.entityId)
            },
        )

        return Pair(actorSystem, masterActor)
    }

    fun leaveCluster(actorSystem: ActorSystem<KbMessage>) {
        Cluster.get(actorSystem).also {
            it.leave(it.selfAddress())
        }
    }
}
