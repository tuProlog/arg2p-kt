package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.term.collectVariables
import it.unibo.tuprolog.argumentation.engine.words.Words
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Cons
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var

/** What selecting a clause of [program] reads of it (see ClauseCode): the keys of its head, and what its code tests first */
internal class ClauseHeads(
    private val program: Program,
) {
    fun keyOf(term: Term): Int =
        when (term) {
            is Var -> 0
            is Atom -> constant(term)
            is Cons -> Words.LIST_KEY
            is Struct -> program.functorWord(term.functor, term.arity)
            else -> constant(term)
        }

    private fun constant(term: Term): Int = program.constantWord(term)

    // See ClauseCode.commitMask. A term against an unbound argument is written, which cannot fail; a variable met again
    // alone, or in the first argument, is a unification that can
    fun commitMaskOf(
        head: Struct,
        body: Term,
    ): Int {
        val leadingCut = isCut(body) || (body is Struct && body.arity == 2 && body.functor == "," && isCut(body[0]))
        if (!leadingCut || head.arity > 31) return -1
        val met = LinkedHashSet<Var>()
        var mask = 0
        for (i in 0 until head.arity) {
            val argument = head[i]
            when {
                argument is Var -> if (!met.add(argument)) return -1
                i == 0 -> {
                    if (argument is Struct && argument !is Atom) {
                        val inner = if (argument is Cons) listOf(argument.head, argument.tail) else List(argument.arity) { argument[it] }
                        for (term in inner) if (term !is Var || !met.add(term)) return -1
                    }
                }
                else -> {
                    mask = mask or (1 shl i)
                    argument.collectVariables(met)
                }
            }
        }
        return mask
    }

    private fun isCut(term: Term): Boolean = term is Atom && term.value == "!"

    fun innerKeysOf(head: Struct): IntArray {
        val first = (if (head.arity > 0) head[0] else null) as? Struct ?: return Program.NO_KEYS
        if (first.arity == 0) return Program.NO_KEYS
        val keys = IntArray(minOf(first.arity, Program.INNER_KEYS)) { keyOf(first[it]) }
        return if (keys.all { it == 0 }) Program.NO_KEYS else keys
    }

    // Whether the code from [start] is tests alone up to a neck cut: on arguments that are where the call left them (a
    // variable moved to a temporary leaves them so)
    fun guardedFrom(
        start: Int,
        arity: Int,
    ): Boolean {
        val code = program.code
        var at = start
        while (true) {
            when (code[at]) {
                Op.CUT_NECK -> return at > start
                Op.GET_CONST_CUT_PROCEED -> return true
                Op.COMPARE_VV, Op.COMPARE_VC -> at += 5 + code[at + 4]
                Op.GET_CONST, Op.GET_X_VAL -> at += 3
                Op.GET_X_VAR -> if (code[at + 1] >= arity) at += 3 else return false
                Op.GET_X_VAR_TYPE_X -> if (code[at + 1] >= arity) at += 5 else return false
                Op.GET_CONST2 -> at += 5
                Op.ARITH_CMP -> at += code[at + 2].let { n -> 4 + n + code[at + 3 + n] }
                Op.TYPE_X -> at += 3
                Op.IDENTICAL, Op.IDENTICAL_C -> at += 4
                else -> return false
            }
        }
    }
}
