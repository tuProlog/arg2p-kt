package it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers

import it.unibo.tuprolog.argumentation.engine.store.ClauseStore

/**
 * What the library keeping its facts in the context tells the context of them, for it to look them up as they are read:
 * the places a predicate is indexed by besides its arguments ([indexed]), the places a lookup names (contextBy/3,
 * contextMatching/3, see [key]) and the relations answered by the goals of that library rather than by facts ([view]).
 * The store gives the layout of the graph (see the module store); with [NONE], facts are looked up by their arguments.
 */
internal interface ContextLayout {
    val indexed: Map<String, List<ClauseStore.Place>>

    /** The place named [name], null if none */
    fun key(name: String): ClauseStore.Place?

    /** The relation answered by goals of [viewModule] for the functor [name], null if none */
    fun view(name: String): View?

    /** The module answering the views */
    val viewModule: String

    /** A relation answered by the goals of [viewModule], [active] for the active branch, [inBranch] for a given one */
    class View(
        val arity: Int,
        val active: String,
        val inBranch: String,
    )

    companion object {
        val NONE: ContextLayout =
            object : ContextLayout {
                override val indexed: Map<String, List<ClauseStore.Place>> = emptyMap()

                override fun key(name: String): ClauseStore.Place? = null

                override fun view(name: String): View? = null

                override val viewModule: String = ""
            }
    }
}
