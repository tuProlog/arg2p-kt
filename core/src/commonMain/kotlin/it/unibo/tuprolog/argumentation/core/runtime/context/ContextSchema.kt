package it.unibo.tuprolog.argumentation.core.runtime.context

/**
 * What the context knows of the terms of the model (see the modules of the data), all here: the store itself only holds
 * clauses. An argument is `argument([Rules, TopRule, [Claim], Body, Info])`.
 */
internal object ContextSchema {
    /** A place inside the clauses of a predicate: an argument of the head and the way from it through list cells, each step 0 for the head of a cell and 1 for its tail */
    class Place(
        val argument: Int,
        val steps: IntArray,
    )

    /** The places a predicate (`functor/arity`) is looked up by, besides its arguments: an argument by its claim */
    val indexed: Map<String, List<Place>> = mapOf("argument/1" to listOf(Place(0, intArrayOf(1, 1, 0, 0))))

    /** The places a lookup names (context_by/3, context_matching/3): the conclusion of an argument, `[Claim]` */
    val keys: Map<String, Place> = mapOf("conclusion" to Place(0, intArrayOf(1, 1, 0)))

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
}
