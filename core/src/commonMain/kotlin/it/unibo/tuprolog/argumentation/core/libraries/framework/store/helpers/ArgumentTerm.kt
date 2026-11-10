package it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers

import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.List as LogicList

/** The term of the module `argument` as Kotlin reads and builds it */
object ArgumentTerm {
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

    /** The parts of the term of an argument, by name */
    class Parts(
        val rules: Term,
        val topRule: Term,
        val conclusion: Term,
        val body: Term,
        val lastDefeasibleRules: Term,
        val defeasibleRules: Term,
        val defeasiblePremises: Term,
    ) {
        /** The term the argument concludes, the single item of its conclusion */
        val claim: Term?
            get() = (conclusion as? LogicList)?.toList()?.singleOrNull()
    }

    /** The parts of [argument], the term of an argument; null if it is not one */
    fun parts(argument: Term): Parts? {
        val items = (argument as? LogicList)?.toList()?.takeIf { it.size == ARGUMENT_PARTS } ?: return null
        val info = (items[INFO_PART] as? LogicList)?.toList()?.takeIf { it.size == INFO_PARTS } ?: return null
        return Parts(
            items[RULES_PART],
            items[TOP_RULE_PART],
            items[CONCLUSION_PART],
            items[BODY_PART],
            info[LAST_DEFEASIBLE_RULES_PART],
            info[DEFEASIBLE_RULES_PART],
            info[DEFEASIBLE_PREMISES_PART],
        )
    }

    /** The term of an argument, made of its parts */
    fun of(
        rules: Term,
        topRule: Term,
        conclusion: Term,
        body: Term,
        info: Term,
    ): Term = LogicList.of(rules, topRule, conclusion, body, info)

    /** The info of an argument, made of its parts */
    fun info(
        lastDefeasibleRules: Term,
        defeasibleRules: Term,
        defeasiblePremises: Term,
    ): Term = LogicList.of(lastDefeasibleRules, defeasibleRules, defeasiblePremises)
}
