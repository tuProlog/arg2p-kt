package it.unibo.tuprolog.argumentation.core.engine.vm

import it.unibo.tuprolog.argumentation.core.engine.vm.Words.ATOM
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.BIG
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.FLT
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.FUN
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.INT
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.LIST
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.payload
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.word
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.theory.Theory

/**
 * A program of the machine (spike): the symbol tables (atoms, functors, floats and large integers, each once, so that
 * equal constants are equal words), the code array the clauses are compiled into (see [Compiler]) and the
 * predicates: those of the theory of the user ([theory]) and, for each set of libraries a resolution runs with, those
 * of the clauses of the libraries and the primitives they provide (a [Scope]).
 */
internal class Program {
    val atoms = ArrayList<String>()
    private val atomIds = HashMap<String, Int>()

    /** The functors: name atom and arity, by id */
    val functorNames = ArrayList<Int>()
    val functorArities = ArrayList<Int>()
    private val functorIds = HashMap<Long, Int>()

    val floats = ArrayList<Double>()
    private val floatIds = HashMap<Double, Int>()
    val bigs = ArrayList<Long>()
    private val bigIds = HashMap<Long, Int>()

    /** The code of the clauses: instructions and their operands */
    var code = IntArray(1 shl 14)
    var codeTop = 0

    /** The highest register any clause uses */
    var maxRegister = 0

    private val compiler = Compiler(this)

    /** The predicates of the theory of the user (its static and dynamic clauses) */
    val theory = Scope(this, null, 0)

    /** The scopes of the libraries, by id (see [scopeOf]) */
    val scopes = ArrayList<Scope>()

    class ClauseCode(
        val key: Int,
        /** The address of the first instruction */
        val start: Int,
        // The clause as given, for the dynamic ones (retracted by equality)
        val source: Clause?,
    )

    /** The clauses of a predicate, and which of them a first argument may match */
    class Predicate(
        val functor: Int,
    ) {
        val clauses = ArrayList<ClauseCode>()
        private var all: IntArray? = null
        private var byKey: HashMap<Int, IntArray>? = null

        /** Bumped whenever the clauses change */
        var version = 0
            private set

        fun add(
            clause: ClauseCode,
            onTop: Boolean,
        ) {
            if (onTop) clauses.add(0, clause) else clauses.add(clause)
            changed()
        }

        fun removeAt(index: Int) {
            clauses.removeAt(index)
            changed()
        }

        private fun changed() {
            all = null
            byKey = null
            version++
        }

        /**
         * The indexes of the clauses a goal whose first argument has [key] may match, in order: all of them for an
         * unbound argument or a small predicate, otherwise those with the same key or none
         */
        fun candidates(key: Int): IntArray {
            if (key == 0 || clauses.size < INDEXED_FROM) return all ?: IntArray(clauses.size) { it }.also { all = it }
            val index = byKey ?: HashMap<Int, IntArray>().also { byKey = it }
            return index.getOrPut(key) {
                val matching = ArrayList<Int>()
                for (i in clauses.indices) {
                    val k = clauses[i].key
                    if (k == 0 || k == key) matching.add(i)
                }
                matching.toIntArray()
            }
        }

        companion object {
            const val INDEXED_FROM = 8
        }
    }

    /**
     * The predicates and primitives a resolution sees with a set of [libraries] (null for the theory of the user):
     * the clauses of the libraries compiled when the scope is made, the primitives looked up as called
     */
    class Scope(
        private val program: Program,
        val libraries: Runtime?,
        val id: Int,
    ) {
        var predicates = arrayOfNulls<Predicate>(1 shl 8)
        private var primitives = arrayOfNulls<Primitive>(0)
        private var looked = BooleanArray(0)

        fun predicate(functor: Int): Predicate? = if (functor < predicates.size) predicates[functor] else null

        // The predicates of the libraries followed by those of the theory of the user, by functor
        private class Merged(
            val libraryVersion: Int,
            val theoryVersion: Int,
            val predicate: Predicate,
        )

        private val merged = HashMap<Int, Merged>()

        /**
         * The predicate [functor] as a resolution sees it when the libraries ([library]) and the theory of the user
         * ([theory]) both define it: the clauses of the libraries first (as the engine concatenates them), kept until
         * either changes
         */
        fun merged(
            functor: Int,
            library: Predicate,
            theory: Predicate,
        ): Predicate {
            val known = merged[functor]
            if (known != null && known.libraryVersion == library.version && known.theoryVersion == theory.version) {
                return known.predicate
            }
            val predicate = Predicate(functor)
            for (clause in library.clauses) predicate.add(clause, onTop = false)
            for (clause in theory.clauses) predicate.add(clause, onTop = false)
            merged[functor] = Merged(library.version, theory.version, predicate)
            return predicate
        }

        /** The primitive of the libraries for [functor], if any */
        fun primitive(functor: Int): Primitive? {
            val libraries = libraries ?: return null
            if (functor >= primitives.size) {
                val size = maxOf(program.functorNames.size, functor + 1)
                primitives = primitives.copyOf(size)
                looked = looked.copyOf(size)
            }
            if (!looked[functor]) {
                looked[functor] = true
                primitives[functor] =
                    libraries.primitives[Signature(program.nameOfFunctor(functor), program.functorArities[functor])]
            }
            return primitives[functor]
        }

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
            val compiled = program.compiler.compile(clause, if (dynamic) clause else null)
            val predicate = predicates[functor] ?: Predicate(functor).also { predicates[functor] = it }
            predicate.add(compiled, onTop)
        }

        /** Removes the first dynamic clause equal to [clause] (variables apart); false if there is none */
        fun remove(clause: Clause): Boolean {
            val head = clause.head ?: return false
            val predicate = predicates[program.functor(head.functor, head.arity)] ?: return false
            val index = predicate.clauses.indexOfFirst { it.source != null && it.source.equals(clause, false) }
            if (index < 0) return false
            predicate.removeAt(index)
            return true
        }
    }

    val emptyList: Int = atomWord("[]")

    fun atom(name: String): Int =
        atomIds.getOrPut(name) {
            atoms.add(name)
            atoms.size - 1
        }

    fun atomWord(name: String): Int = word(ATOM, atom(name))

    fun functor(
        name: String,
        arity: Int,
    ): Int = functor(atom(name), arity)

    fun functor(
        nameAtom: Int,
        arity: Int,
    ): Int {
        val key = (nameAtom.toLong() shl 32) or arity.toLong()
        return functorIds.getOrPut(key) {
            functorNames.add(nameAtom)
            functorArities.add(arity)
            val id = functorNames.size - 1
            val functors = functorNames.size
            theory.grow(functors)
            for (scope in scopes) scope.grow(functors)
            id
        }
    }

    fun functorWord(
        name: String,
        arity: Int,
    ): Int = word(FUN, functor(name, arity))

    fun arity(functorWord: Int): Int = functorArities[payload(functorWord)]

    fun functorName(functorWord: Int): String = atoms[functorNames[payload(functorWord)]]

    fun nameOfFunctor(functor: Int): String = atoms[functorNames[functor]]

    fun floatWord(value: Double): Int =
        word(
            FLT,
            floatIds.getOrPut(value) {
                floats.add(value)
                floats.size - 1
            },
        )

    fun bigWord(value: Long): Int =
        word(
            BIG,
            bigIds.getOrPut(value) {
                bigs.add(value)
                bigs.size - 1
            },
        )

    /** The word of an integer: a small one as it is, a large one by the table */
    fun intWord(value: Long): Int = if (Words.fitsInt(value)) word(INT, value.toInt()) else bigWord(value)

    // ---- code

    /** Appends an instruction word (an opcode or an operand); the address it got */
    fun emit(w: Int): Int {
        if (codeTop == code.size) code = code.copyOf(code.size * 2)
        code[codeTop] = w
        return codeTop++
    }

    fun emit(
        opcode: Int,
        a: Int,
    ) {
        emit(opcode)
        emit(a)
    }

    fun emit(
        opcode: Int,
        a: Int,
        b: Int,
    ) {
        emit(opcode)
        emit(a)
        emit(b)
    }

    /** Adds the clauses of [theory] to those of the theory of the user */
    fun add(theory: Theory) {
        for (clause in theory.clauses) this.theory.add(clause)
    }

    /** Adds [clauses] to those of the theory of the user */
    fun add(clauses: Iterable<Clause>) {
        for (clause in clauses) theory.add(clause)
    }

    /** The scope of [libraries]: made, with their clauses compiled, when first asked for */
    fun scopeOf(libraries: Runtime): Scope {
        for (scope in scopes) if (scope.libraries === libraries) return scope
        val scope = Scope(this, libraries, scopes.size + 1)
        scope.grow(functorNames.size)
        scopes.add(scope)
        for (library in libraries.libraries) for (clause in library.clauses) scope.add(clause)
        return scope
    }

    /** The scope of [id] (see [Scope.id]): the theory of the user for 0 */
    fun scope(id: Int): Scope = if (id == 0) theory else scopes[id - 1]

    /** The code of [query] as a clause whose head holds its variables */
    fun compileQuery(query: Struct): Compiler.Query = compiler.compileQuery(query)

    companion object {
        /** The first-argument key of a list cell */
        const val LIST_KEY = (0 shl Words.TAG_BITS) or LIST
    }
}
