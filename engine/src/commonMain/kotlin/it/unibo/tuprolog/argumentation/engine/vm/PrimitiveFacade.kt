package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.natives.NativeCall.Companion.CONTINUE
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall.Companion.FAIL
import it.unibo.tuprolog.argumentation.engine.vm.PrologExecutionContext
import it.unibo.tuprolog.argumentation.engine.words.ref
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.solve.sideffects.SideEffect

/**
 * A call of a primitive with answers left: its responses, the cells of the variables of its goal, and the error raised
 * computing the next answer ahead, if any (raised when the resolution backtracks into that answer)
 */
internal class PrimitiveCall(
    val responses: Iterator<Solve.Response>,
    val variables: Array<Var>,
    val cells: IntArray,
    var pending: Exception? = null,
)

/**
 * The primitives of the libraries that are not native, called by a machine: their goal as tuProlog terms, their answers
 * unified back, their further answers left as a choice point, their side effects applied to the environment and mirrored
 * into the program.
 */
internal class PrimitiveFacade(
    private val m: Machine,
) {
    // (the heap of the machine, held: one load less on each cell read)
    private val heap = m.heap

    /**
     * Calls [primitive] on the registers: the goal as terms, the first answer unified back, the others left as a choice
     * point resuming at the continuation (the next instruction of a call, see Op.CALL)
     */
    fun call(
        primitive: Primitive,
        functor: Int,
        arity: Int,
    ): Int {
        val names = HashMap<Int, Var>()
        val arguments = List(arity) { heap.toTerm(m.regs[it], names) }
        val name = m.program.nameOfFunctorId(functor)
        val goal = if (arity == 0) Atom.of(name) else Struct.of(name, arguments)
        val context: ExecutionContext = PrologExecutionContext(m.environmentSeen(), procedure = goal)
        val request = Solve.Request(Signature(name, arity), arguments, context)
        val responses =
            try {
                primitive.solve(request).iterator()
            } catch (exception: Exception) {
                throw m.raised(exception)
            }
        val any =
            try {
                responses.hasNext()
            } catch (exception: Exception) {
                throw m.raised(exception)
            }
        if (!any) return FAIL
        val response = nextResponse(responses)
        val variables = arrayOfNulls<Var>(names.size)
        val cells = IntArray(names.size)
        var i = 0
        for ((cell, variable) in names) {
            variables[i] = variable
            cells[i] = cell
            i++
        }
        @Suppress("UNCHECKED_CAST")
        val call = PrimitiveCall(responses, variables as Array<Var>, cells)
        if (hasNextResponse(call)) {
            m.pushChoicePoint(Machine.PRIMITIVE, m.calls.size, 0, 0, m.continuation)
            m.calls.add(call)
        }
        return if (applyResponse(response, call)) CONTINUE else FAIL
    }

    /**
     * Whether [call] has an answer after the current one: computed ahead (to know whether the call leaves alternatives),
     * an error computing it kept for when the resolution backtracks into it
     */
    fun hasNextResponse(call: PrimitiveCall): Boolean =
        try {
            call.responses.hasNext()
        } catch (exception: Exception) {
            call.pending = exception
            true
        }

    fun nextResponse(responses: Iterator<Solve.Response>): Solve.Response =
        try {
            responses.next()
        } catch (exception: Exception) {
            throw m.raised(exception)
        }

    /** The answer of a primitive: its side effects applied, its bindings unified with the cells of the goal */
    fun applyResponse(
        response: Solve.Response,
        call: PrimitiveCall,
    ): Boolean {
        val solution = response.solution
        if (response.sideEffects.isNotEmpty()) applySideEffects(response.sideEffects)
        return when (solution) {
            is Solution.Yes -> {
                val cells = HashMap<Var, Int>(call.variables.size * 2)
                for (i in call.variables.indices) cells[call.variables[i]] = call.cells[i]
                var ok = true
                for ((variable, term) in solution.substitution) {
                    val cell = cells[variable] ?: continue
                    if (!heap.unify(ref(cell), heap.fromTerm(term, cells))) {
                        ok = false
                        break
                    }
                }
                ok
            }
            is Solution.No -> false
            is Solution.Halt -> throw Machine.Raised(m.ballOf(solution.exception), solution.exception)
        }
    }

    private fun applySideEffects(sideEffects: List<SideEffect>) {
        var environment = m.environment
        for (effect in sideEffects) {
            when (effect) {
                is SideEffect.AddDynamicClauses -> {
                    for (clause in effect.clauses) m.program.theory.add(clause, dynamic = true, onTop = effect.onTop)
                    m.fitRegisters()
                }
                is SideEffect.RemoveDynamicClauses -> for (clause in effect.clauses) m.program.theory.remove(clause)
                is SideEffect.ResetDynamicKb -> throw IllegalStateException("side effect not supported by the machine: $effect")
                else -> Unit
            }
            // (the knowledge bases, flags, libraries, operators and channels of the environment, as the effect
            // leaves them: a mutable theory is changed in place)
            environment = effect.applyTo(PrologExecutionContext(environment)).let { it as PrologExecutionContext }.environment
        }
        m.environment = environment
    }
}
