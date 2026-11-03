// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
package it.unibo.tuprolog.argumentation.core.engine.stdlib

import it.unibo.tuprolog.argumentation.core.engine.stdlib.primitive.CopyTerm
import it.unibo.tuprolog.argumentation.core.engine.stdlib.primitive.FindAll
import it.unibo.tuprolog.argumentation.core.engine.stdlib.primitive.Length
import it.unibo.tuprolog.argumentation.core.engine.stdlib.primitive.Throw
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.impl.ExtensionLibrary
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.PrimitiveWrapper
import it.unibo.tuprolog.solve.stdlib.CommonBuiltins

/**
 * The builtins of the solvers of arg2p: those of tuProlog (CommonBuiltins), with the copy_term/2 and findall/3 of the
 * engine in place of tuProlog's (which copy terms by the names of their variables), and length/2 and throw/1 of its
 * own. The control constructs are instructions of the machine (see `engine.vm`).
 */
object DefaultBuiltins : ExtensionLibrary(CommonBuiltins) {
    // copy_term/2 and findall/3 of the engine in place of those of 2p-kt, which copy terms by the names of their
    // variables (see CopyTerm): the library refuses to add a predicate it has, so they replace them here
    override val primitives: Map<Signature, Primitive> by lazy {
        super.primitives + listOf(CopyTerm, FindAll).map { it.descriptionPair }
    }

    override val additionalPrimitives: Iterable<PrimitiveWrapper<*>>
        get() =
            listOf(
                Throw,
                Length,
            )
}
