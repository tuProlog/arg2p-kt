package it.unibo.tuprolog.argumentation.actor

import akka.actor.AddressFromURIString
import akka.actor.CoordinatedShutdown
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

// The node of the cluster of the actor solver on this JVM: the distributor of the knowledge base a singleton of the
// cluster, the workers sharded entities
internal object ClusterInitializer {
    /** The address the nodes bind to unless told otherwise: the local one, for a cluster on a single machine */
    const val LOCAL_HOSTNAME = "127.0.0.1"

    private fun createConfiguration(
        hostname: String,
        port: String,
    ) = ConfigFactory.load(
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
                    hostname = "$hostname"
                    port = $port
                }
            }
            cluster {
                downing-provider-class = "akka.cluster.sbr.SplitBrainResolverProvider"
                # the workers keep their rules (sent once, with Add) only in memory: they are never passivated
                sharding.passivation.strategy = none
            }
            serialization.jackson {
              jackson-modules += "com.fasterxml.jackson.module.kotlin.KotlinModule"
            }
        }
        """,
            ).withFallback(ConfigFactory.load()),
    )

    /**
     * A node of the cluster of the seed node at [seedAddress] (host:port), at [localPort] of [hostname] (the address the
     * other nodes reach it at): its actor system, and the distributor of the knowledge base
     */
    fun joinCluster(
        seedAddress: String,
        localPort: String,
        hostname: String = LOCAL_HOSTNAME,
    ): Pair<ActorSystem<KbMessage>, ActorRef<KbMessage>> {
        val actorSystem: ActorSystem<KbMessage> =
            ActorSystem.create(Behaviors.empty(), "actor_solver", createConfiguration(hostname, localPort))

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

    // Leaves the cluster and then terminates the actor system, freeing its threads and its port: returns once done
    fun leaveCluster(actorSystem: ActorSystem<KbMessage>) {
        CoordinatedShutdown
            .get(actorSystem)
            .runAll(CoordinatedShutdown.clusterLeavingReason())
            .toCompletableFuture()
            .join()
    }
}
