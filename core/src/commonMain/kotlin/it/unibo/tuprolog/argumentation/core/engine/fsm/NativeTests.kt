package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Constant
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Numeric
import it.unibo.tuprolog.core.Real
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.solve.primitive.PrimitiveWrapper
import it.unibo.tuprolog.solve.stdlib.primitive.Atomic
import it.unibo.tuprolog.solve.stdlib.primitive.Callable
import it.unibo.tuprolog.solve.stdlib.primitive.Compound
import it.unibo.tuprolog.solve.stdlib.primitive.Ground
import it.unibo.tuprolog.solve.stdlib.primitive.NonVar
import it.unibo.tuprolog.solve.stdlib.primitive.Atom as AtomTest
import it.unibo.tuprolog.solve.stdlib.primitive.Float as FloatTest
import it.unibo.tuprolog.solve.stdlib.primitive.Integer as IntegerTest
import it.unibo.tuprolog.solve.stdlib.primitive.Number as NumberTest
import it.unibo.tuprolog.solve.stdlib.primitive.Var as VarTest

/**
 * The type tests of the standard library, answered by the engine in the frame of the goal (arg2p): a test binds
 * nothing and raises nothing, and its answer is known at once; solved as a primitive, it cost a frame of its own, a
 * request, the cursor of its answers, a choice point and the frame left. A test is answered this way only where the
 * libraries of the resolution have the standard primitive under its name (any other is called as any primitive).
 */
internal object NativeTests {
    private class Test(
        val standard: PrimitiveWrapper<*>,
        val holds: (Term) -> Boolean,
    )

    // Each test as the standard primitive tests it (a variable is never ground, and not asked)
    private val tests: Map<Signature, Test> =
        listOf(
            Test(VarTest) { it is Var },
            Test(NonVar) { it !is Var },
            Test(Ground) { it !is Var && it.isGround },
            Test(AtomTest) { it is Atom },
            Test(Atomic) { it is Constant },
            Test(Callable) { it is Struct },
            Test(Compound) { it is Struct && it.arity > 0 },
            Test(NumberTest) { it is Numeric },
            Test(IntegerTest) { it is Integer },
            Test(FloatTest) { it is Real },
        ).associateBy { it.standard.signature }

    /** The test of the goals of [signature], if it is one and [libraries] have the standard primitive for it */
    fun of(
        signature: Signature,
        libraries: Runtime,
    ): ((Term) -> Boolean)? {
        val test = tests[signature] ?: return null
        return if (libraries.primitives[signature] === test.standard.implementation) test.holds else null
    }
}
