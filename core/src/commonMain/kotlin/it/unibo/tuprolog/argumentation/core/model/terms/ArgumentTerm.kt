package it.unibo.tuprolog.argumentation.core.model.terms

import it.unibo.tuprolog.argumentation.core.modules.data.ArgumentModule
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.List as LogicList

/** The term of the module ArgumentModule as Kotlin reads and builds it (see [ArgumentModule]) */
object ArgumentTerm {
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
        val items = (argument as? LogicList)?.toList()?.takeIf { it.size == 5 } ?: return null
        val info = (items[4] as? LogicList)?.toList()?.takeIf { it.size == 3 } ?: return null
        return Parts(items[0], items[1], items[2], items[3], info[0], info[1], info[2])
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
