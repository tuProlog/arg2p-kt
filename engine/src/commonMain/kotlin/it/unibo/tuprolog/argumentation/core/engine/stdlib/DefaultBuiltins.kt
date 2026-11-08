package it.unibo.tuprolog.argumentation.core.engine.stdlib

import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.DomainError
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.library.impl.ExtensionLibrary
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.stdlib.CommonBuiltins

/**
 * The builtins of the solvers of arg2p: those of tuProlog (CommonBuiltins), with length/2 and throw/1, which it lacks.
 * The control constructs, findall/3, catch/3 and throw/1 are instructions of the machine, and the primitives it runs on
 * its words are in [it.unibo.tuprolog.argumentation.engine.vm.StdlibNatives]: the entries of this library say that
 * those predicates exist, and answer what the machine leaves to them.
 */
object DefaultBuiltins : ExtensionLibrary(CommonBuiltins) {
    override val primitives: Map<Signature, Primitive> by lazy {
        super.primitives +
            mapOf(
                NativeLength.signature to NativeLength,
                // (never called: the builtin of the machine answers first)
                Signature("throw", 1) to Primitive { throw IllegalStateException("throw/1 is an instruction of the machine") },
            )
    }
}

/**
 * `length(?List, ?Length)`: Length is the number of items of List. A partial list (its tail a variable) is completed
 * with fresh variables to the given length, or, with no length given, to each length in turn from the items it has.
 */
private object NativeLength : AbstractNativePrimitive(Signature("length", 2)) {
    override fun call(call: NativeCall): Int {
        val size = call.arg(1)
        val sizeKind = call.kind(size)
        if (sizeKind != NativeCall.VAR && sizeKind != NativeCall.INT) {
            throw TypeError.forArgument(call.context, signature, TypeError.Expected.INTEGER, call.term(size), 1)
        }
        if (sizeKind == NativeCall.INT && call.intValue(size) < 0) {
            throw DomainError.forArgument(call.context, signature, DomainError.Expected.NOT_LESS_THAN_ZERO, call.term(size), 1)
        }
        var items = 0
        var tail = call.arg(0)
        while (call.kind(tail) == NativeCall.LIST) {
            items++
            tail = call.argOf(tail, 1)
        }
        return when {
            call.kind(tail) == NativeCall.VAR ->
                if (sizeKind == NativeCall.INT) {
                    val length = call.intValue(size).toInt()
                    if (length < items) NativeCall.FAIL else completed(call, tail, length - items)
                } else {
                    each(call, items)
                }
            call.kind(tail) != NativeCall.ATOM || call.atomName(tail) != "[]" ->
                throw TypeError.forArgument(call.context, signature, TypeError.Expected.LIST, call.term(call.arg(0)), 0)
            call.unify(size, call.int(items)) -> NativeCall.CONTINUE
            else -> NativeCall.FAIL
        }
    }

    // The partial list completed to [length] items and to each longer one on backtracking, [length] unified with its
    // size (the list and the size are back in the registers on each answer)
    private fun each(
        call: NativeCall,
        length: Int,
    ): Int {
        var items = 0
        var tail = call.arg(0)
        while (call.kind(tail) == NativeCall.LIST) {
            items++
            tail = call.argOf(tail, 1)
        }
        call.alternatives { each(it, length + 1) }
        return if (completed(call, tail, length - items) == NativeCall.CONTINUE && call.unify(call.arg(1), call.int(length))) {
            NativeCall.CONTINUE
        } else {
            NativeCall.FAIL
        }
    }

    private fun completed(
        call: NativeCall,
        tail: Int,
        fresh: Int,
    ): Int = if (call.unify(tail, call.list(IntArray(fresh) { call.variable() }))) NativeCall.CONTINUE else NativeCall.FAIL
}
