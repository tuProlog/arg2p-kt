package it.unibo.tuprolog.argumentation.core.runtime.context

import it.unibo.tuprolog.argumentation.engine.vm.ClauseStore

/**
 * What the context knows of the terms of the model (see the modules of the data), all here: the store itself only holds
 * clauses. An argument is `argument([Rules, TopRule, [Claim], Body, Info])`.
 */
internal object ContextSchema {
    /** The places a predicate (`functor/arity`) is looked up by, besides its arguments: an argument by its claim */
    val indexed: Map<String, List<ClauseStore.Place>> = mapOf("argument/1" to listOf(ClauseStore.Place(0, intArrayOf(1, 1, 0, 0))))

    /** The places a lookup names (context_by/3, context_matching/3): the conclusion of an argument, `[Claim]`, what was explored */
    val keys: Map<String, ClauseStore.Place> =
        mapOf("conclusion" to ClauseStore.Place(0, intArrayOf(1, 1, 0)), "explored" to ClauseStore.Place(0, IntArray(0)))

    private val conclusionKey = keys.getValue("conclusion")
    private val exploredKey = keys.getValue("explored")

    /** The place named [name] (asked at every lookup by a place: no hash of the name, uncached on JS) */
    fun key(name: String): ClauseStore.Place? =
        when (name) {
            "conclusion" -> conclusionKey
            "explored" -> exploredKey
            else -> null
        }

    /** A relation of the graph kept by the identifiers of its arguments: the goals of [MODULE], of the active branch or of a given one, that answer it */
    class View(
        val arity: Int,
        val active: String,
        val inBranch: String,
    )

    /** The module answering the [views] */
    const val MODULE = "graphStore"

    /** The relations of the graph kept by identifiers, by name: looked up in the context, unless clauses were asserted for them */
    val views: Map<String, View> =
        mapOf(
            "support" to View(2, "support", "supportIn"),
            "attack" to View(4, "attackFound", "attackFoundIn"),
        )

    private val supportView = views.getValue("support")
    private val attackView = views.getValue("attack")

    /** The view of the functor [name] (asked at every lookup: no hash of the name, uncached on JS) */
    fun view(name: String): View? =
        when (name) {
            "support" -> supportView
            "attack" -> attackView
            else -> null
        }
}
