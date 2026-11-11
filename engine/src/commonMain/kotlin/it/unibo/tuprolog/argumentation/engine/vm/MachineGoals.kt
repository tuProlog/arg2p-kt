package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.natives.NativeCall.Companion.CONTINUE
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall.Companion.FAIL
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall.Companion.JUMPED
import it.unibo.tuprolog.argumentation.engine.natives.NativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.RegisterGoalPrimitive
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Builtin
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CHECK_EVERY
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.FRAME
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.INTERRUPTED
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.THEORY
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Halt
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Raised
import it.unibo.tuprolog.argumentation.engine.vm.PrologExecutionContext
import it.unibo.tuprolog.argumentation.engine.words.Words.ATOM
import it.unibo.tuprolog.argumentation.engine.words.Words.FUN
import it.unibo.tuprolog.argumentation.engine.words.Words.LIST
import it.unibo.tuprolog.argumentation.engine.words.Words.REF
import it.unibo.tuprolog.argumentation.engine.words.Words.STR
import it.unibo.tuprolog.argumentation.engine.words.payload
import it.unibo.tuprolog.argumentation.engine.words.smallInt
import it.unibo.tuprolog.argumentation.engine.words.word
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Indicator
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.ResolutionException
import it.unibo.tuprolog.solve.exception.TimeOutException
import it.unibo.tuprolog.solve.exception.warning.MissingPredicate
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.solve.primitive.Primitive
// The calls of goals: of a predicate (its clauses), of a builtin, of a primitive (native or through the facade), of a term,
// of a goal in another module (its scope entered, restored by a block when the goal returns); the housekeeping every so
// many calls (the collection, the deadline, the interruption); a procedure no one defines, as the flag unknown says

/**
 * Calls the predicate, builtin or primitive of [functor] on the registers, [cutBarrier] being the barrier a cut
 * called as the goal itself cuts to: [FAIL], [CONTINUE] (a builtin done, the code goes on at [pc]) or [JUMPED]
 * (at [pc])
 */
internal fun Machine.call(
    functor: Int,
    arity: Int,
): Int {
    if (heap.top > gcThreshold || ++callsSinceCheck >= CHECK_EVERY) housekeeping(arity)
    if (functor < builtinTable.size) {
        val builtin = builtinTable[functor]
        if (builtin != null) return callBuiltin(builtin, functor, arity)
    }
    // (a cut in a clause of the predicate cuts its alternatives alone, whatever the goal is transparent to)
    val predicate = predicateOf(functor) ?: return callPrimitiveOf(functor, arity)
    return if (tryClauses(predicate, arity, cpTop)) JUMPED else FAIL
}

// A call of an instruction: the address to go on at after it, -1 if it failed; [next] the address to come back to,
// negative for a goal in place (the one entry, for the JIT of the JVM to compile what is under it once)
internal fun Machine.goal(
    functor: Int,
    arity: Int,
    next: Int,
): Int {
    settle()
    if (next >= 0) {
        continuation = next
        pc = next
    }
    cutBarrier = cpTop
    return when (call(functor, arity)) {
        FAIL -> -1
        JUMPED -> pc
        else -> if (next >= 0) next else continuation
    }
}

// Every so many calls, or the heap above its threshold: the collection, the deadline and the interruption
internal fun Machine.housekeeping(arity: Int) {
    if (heap.top > gcThreshold) collect(arity)
    if (callsSinceCheck >= CHECK_EVERY) {
        callsSinceCheck = 0
        // (a reading of the time only for a resolution with a limit)
        val chronometer = environment.chronometer
        if (chronometer.checkNow()) {
            val context: ExecutionContext = PrologExecutionContext(environment)
            throw Halt(TimeOutException(exceededDuration = chronometer.timeout, context = context))
        }
        if (cancelled || resolutionInterrupted()) {
            val context: ExecutionContext = PrologExecutionContext(environment)
            throw Halt(ResolutionException(INTERRUPTED, context = context))
        }
    }
}

internal fun Machine.callBuiltin(
    builtin: Builtin,
    functor: Int,
    arity: Int,
): Int {
    // (the clauses the theory of the user gives a builtin are its alternatives: the strict rules of an
    // argumentation theory are clauses of ->/2)
    if (theoryVisible) {
        program.theory.predicate(functor)?.let { pushChoicePoint(functor, 0, arity, cpTop, continuation, 0, THEORY) }
    }
    return builtin.call(this)
}

internal fun Machine.callPrimitiveOf(
    functor: Int,
    arity: Int,
): Int {
    val looked = scope.lookedUp(functor) ?: return unknownProcedure(functor, arity)
    val primitive = looked.primitive ?: return unknownProcedure(functor, arity)
    val native = looked.native
    if (native != null) {
        val answer = native.run(this)
        if (answer != StdlibNatives.NOT_NATIVE) return answer
    }
    return callPrimitive(primitive, functor, arity)
}

// A procedure that neither the clauses nor the libraries define, as the flag unknown says (ISO): an existence error, a
// warning and a failure, or a failure (the solvers of arg2p fail, as a theory may leave its hooks undefined)
internal fun Machine.unknownProcedure(
    functor: Int,
    arity: Int,
): Int {
    val name = program.nameOfFunctorId(functor)
    return when (environment.flags[Unknown] ?: Unknown.defaultValue) {
        Unknown.FAIL -> FAIL
        Unknown.WARNING -> {
            val context = face.context
            context.warnings.write(MissingPredicate(context, Signature(name, arity)))
            FAIL
        }
        else -> throw Raised(error(Struct.of("existence_error", Atom.of("procedure"), Indicator.of(name, arity)), "$name/$arity"))
    }
}

/** Calls the goal [w], a term on the heap, its arguments loaded in the registers */
internal fun Machine.callTerm(w: Int): Int {
    val g = heap.deref(w)
    val functor: Int
    val arity: Int
    // (the compound of a number no word holds is a number, see Numbers)
    when (heap.typeTag(g)) {
        ATOM -> {
            functor = program.atomFunctor(payload(g))
            arity = 0
        }
        STR -> {
            val a = payload(g)
            val f = heap.cells[a]
            functor = payload(f)
            arity = program.arity(f)
            if (arity > regs.size) regs = regs.copyOf(arity * 2)
            for (i in 0 until arity) regs[i] = heap.cells[a + 1 + i]
        }
        LIST -> {
            val a = payload(g)
            functor = fList
            arity = 2
            regs[0] = heap.cells[a]
            regs[1] = heap.cells[a + 1]
        }
        REF -> throw Raised(error(Atom.of("instantiation_error"), "call/1"))
        else -> throw Raised(error(Struct.of("type_error", Atom.of("callable"), heap.toTerm(g, HashMap())), "call/1"))
    }
    return call(functor, arity)
}

// Calls [primitive] on the registers: on its words if native, otherwise through the facade
internal fun Machine.callPrimitive(
    primitive: Primitive,
    functor: Int,
    arity: Int,
): Int = if (primitive is NativePrimitive) callNative(primitive, arity) else primitives.call(primitive, functor, arity)

internal fun Machine.callNative(
    primitive: NativePrimitive,
    arity: Int,
): Int {
    natives.begin(arity)
    return guardingNative { primitive.call(face) }
}

// A module call of the instructions MODULE_CALL and MODULE_EXECUTE, the arguments of the goal in the registers
internal fun Machine.moduleCall(
    kind: Int,
    module: Int,
    functor: Int,
    arity: Int,
): Int {
    moduleGoalFunctor = functor
    moduleGoalArity = arity
    val primitive = scope.primitive(kind) as? RegisterGoalPrimitive
    if (primitive == null) {
        // (no such primitive here: the call as a term, for whatever the resolution makes of it)
        val goal = goalOnRegisters()
        regs[0] = module
        regs[1] = goal
        return call(kind, 2)
    }
    natives.begin(0)
    return guardingNative { primitive.callOnRegisters(face, module) }
}

internal fun Machine.goalOnRegisters(): Int {
    val arity = moduleGoalArity
    val functor = moduleGoalFunctor
    if (arity == 0) return word(ATOM, program.symbols.functorNames[functor])
    val h = heap.alloc(arity + 1)
    heap.cells[h] = word(FUN, functor)
    for (i in 0 until arity) heap.cells[h + 1 + i] = regs[i]
    return word(STR, h)
}

internal fun Machine.enterScope(
    libraries: Runtime,
    theory: Boolean,
) {
    allocate(2)
    local[frame + FRAME] = smallInt(scope.id)
    local[frame + FRAME + 1] = smallInt(if (theoryVisible) 1 else 0)
    continuation = restoreScopeBlock
    scope = program.scopeOf(libraries)
    fitRegisters()
    theoryVisible = theory
    cutBarrier = cpTop
}

// What a goal entered in another scope answers, by what its call did: done at once, it goes on at the block restoring the scope
internal fun Machine.enteredIn(outcome: Int): Int =
    when (outcome) {
        FAIL -> FAIL
        CONTINUE -> jump(continuation)
        else -> JUMPED
    }
