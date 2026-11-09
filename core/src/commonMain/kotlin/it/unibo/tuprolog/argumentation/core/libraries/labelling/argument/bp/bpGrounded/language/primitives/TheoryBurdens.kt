package it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.bp.bpGrounded.language.primitives

import it.unibo.tuprolog.argumentation.core.libraries.language.parser.helpers.TheoryClauses
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.toTerm
import it.unibo.tuprolog.solve.Signature

/**
 * theoryBurdens(-Bps): the burdens of persuasion of the theory, its `bp(...)` facts, as `abstractBp([...])` terms
 */
internal object TheoryBurdens : AbstractNativePrimitive(Signature("theoryBurdens", 1)) {
    override fun call(call: NativeCall): Int =
        TheoryClauses.reply(call, signature) { clauses, prologScope ->
            clauses
                .filter { it.value.isFact && it.value.head?.functor == "bp" }
                .map { Struct.of("abstractBp", prologScope.logicListOf(it.value.head!!.args)) }
                .toTerm()
        }
}
