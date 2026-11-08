package it.unibo.tuprolog.argumentation.core.primitives.graph.language

import it.unibo.tuprolog.argumentation.core.flags.PrologStrictCompatibility
import it.unibo.tuprolog.argumentation.core.runtime.settings.Settings
import it.unibo.tuprolog.argumentation.core.runtime.signatureOf
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.reply
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.dsl.LogicProgrammingScope
import it.unibo.tuprolog.dsl.logicProgramming
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError

internal object ConversionUtils {
    fun modifiers(
        target: Clause,
        context: ExecutionContext,
    ): Term =
        logicProgramming {
            target.bodyItems
                .map { term ->
                    if (term.isStruct && term.asStruct()?.functor == "\\+") {
                        "~"(term.asStruct()!!.args[0])
                    } else if (isPrologGoal(term, context)) {
                        "prolog"(term)
                    } else {
                        term
                    }
                }.let { if (it.count() > 1) tupleOf(it) else it.first() }
        }

    /**
     * Whether a goal of a rule is a Prolog goal: one clause/2 cannot inspect (a variable, a term that is not
     * callable, or a procedure the libraries protect), rather than a statement of the theory
     */
    private fun isPrologGoal(
        goal: Term,
        context: ExecutionContext,
    ): Boolean = goal !is Struct || context.libraries.hasProtected(goal.signatureOf)

    /** The identifier of the rule a Prolog clause stands for: from its position in the theory, the same at each conversion */
    fun ruleId(index: Int): String = "rule_$index"

    // The functors of the facts written in the language of arg2p (rules, premises, axioms), not Prolog facts
    private val arg2pFacts = setOf(":->", "->", "=>", ":=>", ":", ",")

    /** Whether [clause] is a Prolog fact, rather than a rule, a premise or an axiom written in the language of arg2p */
    fun isPrologFact(clause: Clause): Boolean = clause.isFact && clause.head!!.functor !in arg2pFacts

    /**
     * The answer of a conversion of the theory: the first argument of [call] (a variable, a type error otherwise)
     * unified with what [mapper] makes of the clauses of the theory, if prologStrictCompatibility is on (or [force]),
     * the empty list otherwise
     */
    fun commonMap(
        call: NativeCall,
        signature: Signature,
        force: Boolean = false,
        mapper: (Iterable<IndexedValue<Clause>>, LogicProgrammingScope) -> Term,
    ): Int {
        val target = call.arg(0)
        if (call.kind(target) != NativeCall.VAR) {
            throw TypeError.forArgument(call.context, signature, TypeError.Expected.VARIABLE, call.term(target), 0)
        }
        val context = call.context
        val term =
            logicProgramming {
                if (force || Settings.of(context)?.isOn(PrologStrictCompatibility, context) == true) {
                    mapper(context.staticKb.clauses.withIndex(), this)
                } else {
                    emptyLogicList
                }
            }
        return call.reply(0, term)
    }
}
