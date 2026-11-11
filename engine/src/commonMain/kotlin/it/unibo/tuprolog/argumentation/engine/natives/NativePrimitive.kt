package it.unibo.tuprolog.argumentation.engine.natives

import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * A primitive native to the machine: [call] runs it on its arguments as words of the machine, read, unified and
 * built through the [NativeCall], without materialising them as terms (the facade does that for any other
 * [Primitive]: the goal built as a term, its answer taken back as a substitution). A [Primitive] still, for the
 * solvers that are not the machine: [AbstractNativePrimitive] gives [solve] for free, over a [NativeCall] backed
 * by terms. A tuProlog exception thrown by [call] is raised as the facade raises it.
 */
interface NativePrimitive : Primitive {
    /** Runs the primitive: [NativeCall.FAIL], [NativeCall.CONTINUE] (done) or [NativeCall.JUMPED] (a goal was entered) */
    fun call(call: NativeCall): Int
}

/**
 * The base of a primitive written once, against the [NativeCall]: the machine runs [call] on its words, any other
 * solver runs it over the terms of the request ([solve], which a primitive with a better term-level resolution
 * may override). The helpers of `NativeCalls.kt` read the arguments, reply and give answers.
 */
abstract class AbstractNativePrimitive(
    override val signature: Signature,
) : NativePrimitive,
    PrimitiveWithSignature {
    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> = TermCall.solve(this, request)
}
