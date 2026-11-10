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
    /**
     * The predicates by functor: for the theory, as many as the functors there are; for libraries, up to the highest
     * functor they define, the table made once (see Program.scopeOf), and read only, shared by every program of a base
     * when the code of each library is the base's
     */
    var predicates: Array<Predicate?> = if (libraries == null) arrayOfNulls(1 shl 8) else NO_PREDICATES

    /** What the function of the libraries asked for last gave, and that function (see MachineCall.libraryValue) */
    var libraryValue: Any? = null
    var libraryValueOf: ((Runtime) -> Any?)? = null

    // What each functor looked up gave (see lookedUp): as many as the highest functor looked up, a few in each scope
    private var looked = arrayOfNulls<Looked>(0)

    fun predicate(functor: Int): Predicate? = if (functor < predicates.size) predicates[functor] else null

    /** The primitive of the libraries for [functor], if any */
    fun primitive(functor: Int): Primitive? = lookedUp(functor)?.primitive

    /** The primitive of the libraries for [functor] and the native standing in for it, looked up once (null for the theory) */
    fun lookedUp(functor: Int): Looked? {
        if (libraries == null) return null
        if (functor >= looked.size) looked = looked.copyOf(functor + 1 + (functor ushr 3))
        looked[functor]?.let { return it }
        val signature = Signature(program.nameOfFunctorId(functor), program.symbols.functorArities[functor])
        val primitive = primitiveOf(signature)
        return (if (primitive == null) Looked.NONE else Looked(primitive, StdlibNatives.of(signature, primitive)))
            .also { looked[functor] = it }
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

    // (the theory only: the table of libraries is made whole, and a functor beyond it has no predicate there)
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

    companion object {
        private val NO_PREDICATES = arrayOfNulls<Predicate>(0)

        /**
         * The predicates of the libraries of [codes] by functor, up to the highest they define: those of one library as they
         * are, those of several joined in a new one, frozen if [frozen] (the table read by the programs of a base, any thread)
         */
        internal fun tableOf(
            codes: List<LibraryCode>,
            frozen: Boolean,
        ): Array<Predicate?> {
            var size = 0
            for (code in codes) for (predicate in code.predicates) if (predicate.functor >= size) size = predicate.functor + 1
            val table = arrayOfNulls<Predicate>(size)
            var joined = false
            for (code in codes) {
                for (predicate in code.predicates) {
                    val functor = predicate.functor
                    val present = table[functor]
                    table[functor] =
                        if (present == null) {
                            predicate
                        } else {
                            joined = true
                            Predicate(functor).also { both ->
                                present.clauses.forEach { both.add(it, false) }
                                predicate.clauses.forEach { both.add(it, false) }
                            }
                        }
                }
            }
            if (frozen && joined) {
                val own = HashSet<Predicate>()
                for (code in codes) own.addAll(code.predicates)
                for (predicate in table) if (predicate != null && predicate !in own) predicate.freeze()
            }
            return table
        }
    }
}

/** What the libraries of a scope have for a functor: its primitive, and the native standing in for it, if any */
internal class Looked(
    val primitive: Primitive?,
    val native: StdlibNative?,
) {
    companion object {
        /** No primitive */
        val NONE = Looked(null, null)
    }
}

/** The code of a library: its predicates, where the code of its clauses ends, and whether it is a base's ([frozen]) */
internal class LibraryCode(
    val predicates: List<Predicate>,
    val codeEnd: Int,
    val frozen: Boolean,
)
