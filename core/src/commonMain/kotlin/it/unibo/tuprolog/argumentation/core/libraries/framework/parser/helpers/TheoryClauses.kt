package it.unibo.tuprolog.argumentation.core.libraries.framework.parser.helpers

import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.reply
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.dsl.LogicProgrammingScope
import it.unibo.tuprolog.dsl.logicProgramming
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError

/** For the primitives reading the clauses of a theory */
internal object TheoryClauses {
    /** The first argument of [call] (a variable, a type error otherwise) unified with what [mapper] makes of the clauses of the theory */
    fun reply(
        call: NativeCall,
        signature: Signature,
        mapper: (Iterable<IndexedValue<Clause>>, LogicProgrammingScope) -> Term,
    ): Int {
        val target = call.arg(0)
        if (call.kind(target) != NativeCall.VAR) {
            throw TypeError.forArgument(call.context, signature, TypeError.Expected.VARIABLE, call.term(target), 0)
        }
        val context = call.context
        val term = logicProgramming { mapper(context.staticKb.clauses.withIndex(), this) }
        return call.reply(0, term)
    }
}
