package it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers

import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.ContextLayout
import it.unibo.tuprolog.argumentation.engine.store.ClauseStore
import it.unibo.tuprolog.argumentation.engine.store.Record
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term

/**
 * The facts of the graph in the context, as the module store keeps them, and what it tells the context of them (see
 * [ContextLayout]): an argument is `argument([Rules, TopRule, [Claim], Body, Info])` (see [ArgumentTerm]), looked up by its
 * claim, and the relations of the graph kept by identifiers are answered by the goals of the store. The facts of the
 * exploration of a query (explored/1, see the module queryGraph) are among them, as the solvers of a session share
 * them through the store.
 */
internal object StoreSchema : ContextLayout {
    /** The facts of the graph, as the module store keeps them: `arg(Id)` */
    const val SAVED = "arg"

    /** `argInfo(Id, InfoId)` */
    const val ARGUMENT_INFO = "argInfo"

    /** `supportId(IdSupporter, IdSupported)` */
    const val SUPPORT = "supportId"

    /** `attackId(Type, IdAttacker, IdAttacked, IdOn)` */
    const val ATTACK = "attackId"

    /** `newConc(Tag, Conclusion)` */
    const val NEW_CONCLUSION = "newConc"

    /** `explored(Statement)` (the module queryGraph) */
    const val EXPLORED = "explored"

    const val SAVED_ARITY = 1
    const val ARGUMENT_INFO_ARITY = 2
    const val SUPPORT_ARITY = 2
    const val ATTACK_ARITY = 4
    const val NEW_CONCLUSION_ARITY = 2
    const val EXPLORED_ARITY = 1

    /** The predicates values derived from them are kept for (see [ClauseStore.derived]) */
    const val SAVED_PREDICATE = "$SAVED/$SAVED_ARITY"
    const val SUPPORT_PREDICATE = "$SUPPORT/$SUPPORT_ARITY"
    const val ATTACK_PREDICATE = "$ATTACK/$ATTACK_ARITY"

    /** The fact holding [argument], the term of an argument: what its identifier is of */
    fun argument(argument: Term): Struct = Struct.of("argument", argument)

    /** The fact holding [info], the info of an argument: what its identifier is of */
    fun info(info: Term): Struct = Struct.of("info", info)

    /** The word of the conclusion of the record of `argument(A)` */
    fun conclusionWord(argument: Record): Int = argument.itemAt(argument.argumentWord(0), ArgumentTerm.CONCLUSION_PART)

    /** The places a predicate (`functor/arity`) is looked up by, besides its arguments: an argument by its claim */
    override val indexed: Map<String, List<ClauseStore.Place>> = mapOf("argument/1" to listOf(ClauseStore.Place(0, intArrayOf(1, 1, 0, 0))))

    /** The places a lookup names (contextBy/3, contextMatching/3): the conclusion of an argument, `[Claim]`, what was explored */
    val keys: Map<String, ClauseStore.Place> =
        mapOf("conclusion" to ClauseStore.Place(0, intArrayOf(1, 1, 0)), "explored" to ClauseStore.Place(0, IntArray(0)))

    private val conclusionKey = keys.getValue("conclusion")
    private val exploredKey = keys.getValue("explored")

    /** The place named [name] (asked at every lookup by a place: no hash of the name, uncached on JS) */
    override fun key(name: String): ClauseStore.Place? =
        when (name) {
            "conclusion" -> conclusionKey
            "explored" -> exploredKey
            else -> null
        }

    override val viewModule: String = "store"

    /** The relations of the graph kept by identifiers, by name: looked up in the context, unless clauses were asserted for them */
    private val views: Map<String, ContextLayout.View> =
        mapOf(
            "support" to ContextLayout.View(2, "support", "supportIn"),
            "attack" to ContextLayout.View(4, "attackFound", "attackFoundIn"),
        )

    private val supportView = views.getValue("support")
    private val attackView = views.getValue("attack")

    /** The view of the functor [name] (asked at every lookup: no hash of the name, uncached on JS) */
    override fun view(name: String): ContextLayout.View? =
        when (name) {
            "support" -> supportView
            "attack" -> attackView
            else -> null
        }
}
