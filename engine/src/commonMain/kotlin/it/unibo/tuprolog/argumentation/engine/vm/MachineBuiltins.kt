package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.natives.NativeCall.Companion.CONTINUE
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall.Companion.FAIL
import it.unibo.tuprolog.argumentation.engine.vm.program.Op
import it.unibo.tuprolog.argumentation.engine.vm.program.Program
import it.unibo.tuprolog.argumentation.engine.words.Words.REF
import it.unibo.tuprolog.argumentation.engine.words.Words.STR
import it.unibo.tuprolog.argumentation.engine.words.payload
import it.unibo.tuprolog.argumentation.engine.words.tag
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Indicator
import it.unibo.tuprolog.core.Struct

/**
 * The builtins of the machines of [program], by functor: the control constructs called as goals, =/2, the arithmetic and the
 * type tests the machine runs on its words, findall/3, catch/3 and throw/1
 */
internal fun builtinsOf(program: Program): Array<Machine.Builtin?> {
    var table = arrayOfNulls<Machine.Builtin>(1 shl 8)
    val blocks = program.controlBlocks()
    val ifThen = program.functorWord("->", 2)
    val fail = program.atomWord("fail")

    fun put(
        functor: Int,
        implementation: Machine.Builtin,
    ) {
        if (functor >= table.size) table = table.copyOf(functor + (1 shl 8))
        table[functor] = implementation
    }

    fun builtin(
        name: String,
        arity: Int,
        implementation: Machine.Builtin,
    ) = put(program.functor(name, arity), implementation)

    // An arithmetic builtin: [native] on the integers and the operators the machine evaluates itself, the primitive
    // of the libraries (tuProlog's evaluation, floats and every function) on anything else
    fun arithmetic(
        name: String,
        native: Machine.Builtin,
    ) {
        val functor = program.functor(name, 2)
        put(
            functor,
            Machine.Builtin { m ->
                try {
                    native.call(m)
                } catch (_: Machine.NotNative) {
                    val primitive =
                        m.scope.primitive(functor)
                            ?: throw Machine.Raised(
                                m.error(Struct.of("existence_error", Atom.of("procedure"), Indicator.of(name, 2)), "$name/2"),
                            )
                    m.callPrimitive(primitive, functor, 2)
                }
            },
        )
    }
    builtin("true", 0) { CONTINUE }
    builtin("fail", 0) { FAIL }
    builtin("false", 0) { FAIL }
    builtin("!", 0) { m ->
        m.cutTo(m.cutBarrier)
        CONTINUE
    }
    builtin("=", 2) { m -> if (m.heap.unify(m.regs[0], m.regs[1])) CONTINUE else FAIL }
    arithmetic("is") { m -> if (m.heap.unify(m.regs[0], m.heap.intWord(m.arithmetic.eval(m.regs[1])))) CONTINUE else FAIL }
    for ((name, kind) in Op.COMPARISONS) {
        arithmetic(
            name,
        ) { m -> if (holds(kind, m.arithmetic.eval(m.regs[0]).compareTo(m.arithmetic.eval(m.regs[1])), 0)) CONTINUE else FAIL }
    }
    builtin("var", 1) { m -> if (tag(m.heap.deref(m.regs[0])) == REF) CONTINUE else FAIL }
    builtin("nonvar", 1) { m -> if (tag(m.heap.deref(m.regs[0])) != REF) CONTINUE else FAIL }
    builtin("call", 1) { m ->
        m.cutBarrier = m.cpTop
        m.callTerm(m.regs[0])
    }
    builtin(",", 2) { m -> m.jump(blocks.conjunction) }
    builtin(";", 2) { m ->
        val left = m.heap.deref(m.regs[0])
        if (tag(left) == STR && m.heap.cells[payload(left)] == ifThen) {
            val a = payload(left)
            m.regs[2] = m.regs[1]
            m.regs[0] = m.heap.cells[a + 1]
            m.regs[1] = m.heap.cells[a + 2]
            m.jump(blocks.ifThenElse)
        } else {
            m.jump(blocks.disjunction)
        }
    }
    builtin("->", 2) { m ->
        m.regs[2] = fail
        m.jump(blocks.ifThenElse)
    }
    builtin("\\+", 1) { m -> m.jump(blocks.negation) }
    builtin("findall", 3) { m -> m.nested.findall(m.regs[0], m.regs[1], m.regs[2]) }
    builtin("catch", 3) { m -> m.catch() }
    builtin("\$catch_exit", 1) { m -> m.catchExit() }
    builtin("throw", 1) { m ->
        val ball = m.heap.deref(m.regs[0])
        if (tag(ball) == REF) throw Machine.Raised(m.error(Atom.of("instantiation_error"), "throw/1"))
        throw Machine.Raised(m.heap.toTerm(ball, HashMap()))
    }
    return table
}
