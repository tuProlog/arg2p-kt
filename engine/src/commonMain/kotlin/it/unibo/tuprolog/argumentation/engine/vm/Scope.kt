package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The predicates and primitives a resolution sees with a set of [libraries] (null for the theory of the user):
 * the clauses of the libraries compiled when the scope is made, the primitives looked up as called
 */
internal class Scope(
    private val program: Program,
    val libraries: Runtime?,
    val id: Int,
) {
    var predicates = arrayOfNulls<Predicate>(1 shl 8)

    /** What the function of the libraries asked for last gave, and that function (see MachineCall.libraryValue) */
    var libraryValue: Any? = null
    var libraryValueOf: ((Runtime) -> Any?)? = null

    private var primitives = arrayOfNulls<Primitive>(0)
    private var natives = arrayOfNulls<StdlibNative>(0)
    private var looked = BooleanArray(0)

    fun predicate(functor: Int): Predicate? = if (functor < predicates.size) predicates[functor] else null

    /** The primitive of the libraries for [functor], if any */
    fun primitive(functor: Int): Primitive? {
        val libraries = libraries ?: return null
        if (functor >= primitives.size) {
            val size = maxOf(primitives.size * 2, program.symbols.functorNames.size, functor + 1)
            primitives = primitives.copyOf(size)
            natives = natives.copyOf(size)
            looked = looked.copyOf(size)
        }
        if (!looked[functor]) {
            looked[functor] = true
            val signature = Signature(program.nameOfFunctorId(functor), program.symbols.functorArities[functor])
            primitives[functor] = primitiveOf(signature)
            natives[functor] = StdlibNatives.of(signature, primitives[functor])
        }
        return primitives[functor]
    }

    // As the map of the primitives of all the libraries would say, last library first, with no such map made for each
    // scope; only a name qualified by a library needs it
    private var primitiveMaps: List<Map<Signature, Primitive>>? = null

    private fun primitiveOf(signature: Signature): Primitive? {
        val libraries = libraries ?: return null
        val maps =
            primitiveMaps ?: libraries.libraries
                .map { it.primitives }
                .filter { it.isNotEmpty() }
                .reversed()
                .also { primitiveMaps = it }
        for (map in maps) map[signature]?.let { return it }
        return if (signature.name.length > 1 && Library.ALIAS_SEPARATOR in signature.name) libraries.primitives[signature] else null
    }

    /** The native standing in for the primitive of [functor], looked up by [primitive] */
    fun native(functor: Int): StdlibNative? = if (functor < natives.size) natives[functor] else null

    internal fun grow(functors: Int) {
        if (functors > predicates.size) predicates = predicates.copyOf(maxOf(predicates.size * 2, functors))
    }

    /** Adds [clause], last among those of its predicate (first if [onTop]); [dynamic] ones can be removed */
    fun add(
        clause: Clause,
        dynamic: Boolean = false,
        onTop: Boolean = false,
    ) {
        val head = clause.head ?: return
        val functor = program.functor(head.functor, head.arity)
        val compiled = program.compiler.compile(clause, dynamic)
        grow(program.symbols.functorNames.size)
        val predicate = predicates[functor] ?: Predicate(functor).also { predicates[functor] = it }
        predicate.add(compiled, onTop)
        if (this === program.theory) program.theoryVersion++
    }

    /** The predicates of a library added to the scope: shared as they are, those it has already joined in a new one */
    internal fun merge(code: LibraryCode) {
        grow(program.symbols.functorNames.size)
        for (predicate in code.predicates) {
            val functor = predicate.functor
            val present = predicates[functor]
            predicates[functor] =
                if (present == null) {
                    predicate
                } else {
                    Predicate(functor).also { joined ->
                        present.clauses.forEach { joined.add(it, false) }
                        predicate.clauses.forEach { joined.add(it, false) }
                    }
                }
        }
    }

    /** Removes the first dynamic clause equal to [clause] (variables apart); false if there is none */
    fun remove(clause: Clause): Boolean {
        val head = clause.head ?: return false
        val predicate = predicates[program.functor(head.functor, head.arity)] ?: return false
        val index = predicate.clauses.indexOfFirst { it.dynamic && it.source.equals(clause, false) }
        if (index < 0) return false
        predicate.removeAt(index)
        if (this === program.theory) program.theoryVersion++
        return true
    }
}

/** The code of a library: its predicates, and where the code of its clauses ends */
internal class LibraryCode(
    val predicates: List<Predicate>,
    val codeEnd: Int,
)
