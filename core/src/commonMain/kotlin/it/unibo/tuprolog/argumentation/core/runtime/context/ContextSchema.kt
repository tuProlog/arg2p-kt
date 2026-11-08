package it.unibo.tuprolog.argumentation.core.runtime.context

import it.unibo.tuprolog.argumentation.engine.vm.ClauseStore
import it.unibo.tuprolog.argumentation.engine.vm.Record
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term

/**
 * What the context knows of the terms of the model (see the modules of the data), all here: the store itself only holds
 * clauses. An argument is `argument([Rules, TopRule, [Claim], Body, Info])`.
 */
internal object ContextSchema {
    /** The facts of the graph, as the module graphStore keeps them: `arg(Id)` */
    const val SAVED = "arg"

    /** `argInfo(Id, InfoId)` */
    const val ARGUMENT_INFO = "argInfo"

    /** `supportId(IdSupporter, IdSupported)` */
    const val SUPPORT = "supportId"

    /** `attackId(Type, IdAttacker, IdAttacked, IdOn)` */
    const val ATTACK = "attackId"

    /** `newConc(Tag, Conclusion)` */
    const val NEW_CONCLUSION = "newConc"

    /** `explored(Statement)` (the module queryExploration) */
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

    /** The places of the parts of an argument in its list */
    const val RULES_PART = 0
    const val TOP_RULE_PART = 1
    const val CONCLUSION_PART = 2
    const val BODY_PART = 3
    const val INFO_PART = 4
    const val ARGUMENT_PARTS = 5

    /** The places of the parts of the info of an argument in its list */
    const val LAST_DEFEASIBLE_RULES_PART = 0
    const val DEFEASIBLE_RULES_PART = 1
    const val DEFEASIBLE_PREMISES_PART = 2
    const val INFO_PARTS = 3

    /** The fact holding [argument], the term of an argument: what its identifier is of */
    fun argument(argument: Term): Struct = Struct.of("argument", argument)

    /** The fact holding [info], the info of an argument: what its identifier is of */
    fun info(info: Term): Struct = Struct.of("info", info)

    /** The word of the conclusion of the record of `argument(A)` */
    fun conclusionWord(argument: Record): Int = argument.itemAt(argument.argumentWord(0), CONCLUSION_PART)

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
