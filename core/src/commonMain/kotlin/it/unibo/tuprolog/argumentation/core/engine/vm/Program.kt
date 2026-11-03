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
import it.unibo.tuprolog.core.Real
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.theory.Theory

/**
 * A program of the machine (spike): the symbol tables (atoms, functors, floats and large integers, each once, so that
 * equal constants are equal words), the code array the clauses are compiled into (see [Compiler]) and the
 * predicates: those of the theory of the user ([theory]) and, for each set of libraries a resolution runs with, those
 * of the clauses of the libraries and the primitives they provide (a [Scope]).
 */
internal class Program() {
    val atoms = ArrayList<String>()
    private val atomIds = HashMap<String, Int>()

    /** The functors: name atom and arity, by id */
    val functorNames = ArrayList<Int>()
    val functorArities = ArrayList<Int>()
    private val functorIds = HashMap<Long, Int>()

    // (the floats as tuProlog reads them: a float read from "61.167" and the double 61.167 are two values, and the
    // first must come out as it went in)
    val floats = ArrayList<Real>()
    private val floatIds = HashMap<Real, Int>()
    val bigs = ArrayList<Long>()
    private val bigIds = HashMap<Long, Int>()

    /** The code of the clauses: instructions and their operands */
    var code = IntArray(1 shl 14)
    var codeTop = 0

    /** The highest register any clause uses */
    var maxRegister = 0

    // The program this one was derived from, if any (see SharedPrograms): its symbols and its code up to
    // [baseCodeTop] are this program's too, so the scopes it compiled by then serve here as they are
    private var base: Program? = null
    private var baseCodeTop = 0

    // The scopes of a base program by the aliases of their libraries, each with the libraries (with clauses) it was
    // compiled from and where its code ends
    private val sharedScopes = HashMap<List<String>, Scope>()

    /** A program starting as a copy of [base]: its symbols, its code, its scopes (compiled once, for every solver) */
    constructor(base: Program) : this() {
        // (the tables as the base has them: those the primary constructor made are its first entries too)
        atoms.clear()
        atoms.addAll(base.atoms)
        atomIds.clear()
        atomIds.putAll(base.atomIds)
        functorNames.clear()
        functorNames.addAll(base.functorNames)
        functorArities.clear()
        functorArities.addAll(base.functorArities)
        functorIds.clear()
        functorIds.putAll(base.functorIds)
        floats.clear()
        floats.addAll(base.floats)
        floatIds.clear()
        floatIds.putAll(base.floatIds)
        bigs.clear()
        bigs.addAll(base.bigs)
        bigIds.clear()
        bigIds.putAll(base.bigIds)
        code = base.code.copyOf(maxOf(base.codeTop + (1 shl 12), 1 shl 14))
        codeTop = base.codeTop
        maxRegister = base.maxRegister
        this.base = base
        baseCodeTop = base.codeTop
        theory.grow(functorNames.size)
    }

    private val compiler = Compiler(this)

    /** The predicates of the theory of the user (its static and dynamic clauses) */
    val theory = Scope(this, null, 0)

    /** The scopes of the libraries, by id (see [scopeOf]) */
    val scopes = ArrayList<Scope>()

    class ClauseCode(
        /** The keys of the arguments of the head (0: a variable, matching anything); [key] is the first */
        val keys: IntArray,
        /** The address of the first instruction */
        val start: Int,
        /** The clause as given */
        val source: Clause,
        /** Whether the clause is dynamic (retracted by equality) */
        val dynamic: Boolean,
    ) {
        val key: Int get() = if (keys.isEmpty()) 0 else keys[0]

        /** Whether an argument beyond the first is keyed */
        val deep: Boolean get() = keys.size > 1 && (1 until keys.size).any { keys[it] != 0 }
    }

    /** The clauses of a predicate, and which of them a first argument may match */
    class Predicate(
        val functor: Int,
    ) {
        val clauses = ArrayList<ClauseCode>()
        private var all: IntArray? = null
        private var byKey: HashMap<Int, IntArray>? = null

        /** The first-argument key and the start of each clause, flat, for the selection to read */
        var firstKeys = IntArray(4)
            private set
        var starts = IntArray(4)
            private set

        /** Whether a clause keys an argument beyond the first */
        var deep = false
            private set

        /** How many clauses there are */
        var size = 0
            private set

        /** Bumped whenever the clauses change */
        var version = 0
            private set

        fun add(
            clause: ClauseCode,
            onTop: Boolean,
        ) {
            val n = clauses.size
            if (n == firstKeys.size) {
                firstKeys = firstKeys.copyOf(n * 2)
                starts = starts.copyOf(n * 2)
            }
            val at = if (onTop) 0 else n
            if (onTop) {
                clauses.add(0, clause)
                firstKeys.copyInto(firstKeys, 1, 0, n)
                starts.copyInto(starts, 1, 0, n)
            } else {
                clauses.add(clause)
            }
            firstKeys[at] = clause.key
            starts[at] = clause.start
            if (clause.deep) deep = true
            size = clauses.size
            changed()
        }

        fun removeAt(index: Int) {
            val n = clauses.size
            val removed = clauses.removeAt(index)
            firstKeys.copyInto(firstKeys, index, index + 1, n)
            starts.copyInto(starts, index, index + 1, n)
            if (removed.deep) deep = clauses.any { it.deep }
            size = clauses.size
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
        predicates: Array<Predicate?> = arrayOfNulls(1 shl 8),
    ) {
        var predicates = predicates

        /** Where the code of the clauses compiled for the scope ends (a scope of a base program) */
        var codeEnd = 0

        /** The libraries with clauses the scope was compiled from, for another program to check before sharing it */
        var sources: List<Library> = emptyList()
        private var primitives = arrayOfNulls<Primitive>(0)
        private var looked = BooleanArray(0)

        fun predicate(functor: Int): Predicate? = if (functor < predicates.size) predicates[functor] else null

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
            val compiled = program.compiler.compile(clause, dynamic)
            grow(program.functorNames.size)
            val predicate = predicates[functor] ?: Predicate(functor).also { predicates[functor] = it }
            predicate.add(compiled, onTop)
        }

        /** Removes the first dynamic clause equal to [clause] (variables apart); false if there is none */
        fun remove(clause: Clause): Boolean {
            val head = clause.head ?: return false
            val predicate = predicates[program.functor(head.functor, head.arity)] ?: return false
            val index = predicate.clauses.indexOfFirst { it.dynamic && it.source.equals(clause, false) }
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
            for (scope in sharedScopes.values) scope.grow(functors)
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

    fun floatWord(value: Real): Int =
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

    fun emit(
        opcode: Int,
        a: Int,
        b: Int,
        c: Int,
    ) {
        emit(opcode)
        emit(a)
        emit(b)
        emit(c)
    }

    /** The operand naming a variable the code creates: its name atom and one, 0 for an anonymous one */
    fun nameOperand(variable: Var): Int = if (variable.isAnonymous) 0 else atom(variable.name) + 1

    /** Adds the clauses of [theory] to those of the theory of the user */
    fun add(theory: Theory) {
        for (clause in theory.clauses) this.theory.add(clause)
    }

    /** Adds [clauses] to those of the theory of the user */
    fun add(clauses: Iterable<Clause>) {
        for (clause in clauses) theory.add(clause)
    }

    /**
     * The scope of [libraries]: made when first asked for, its clauses compiled, or taken from the base program when
     * it compiled a scope of the same libraries (the clauses of a module are the same objects in every solver)
     */
    fun scopeOf(libraries: Runtime): Scope {
        for (scope in scopes) if (scope.libraries === libraries) return scope
        val base = base
        if (base != null) {
            val shared = base.sharedScopeOf(libraries)
            if (shared != null && shared.codeEnd <= baseCodeTop) {
                val scope = Scope(this, libraries, scopes.size + 1, shared.predicates)
                scope.grow(functorNames.size)
                scopes.add(scope)
                return scope
            }
        }
        val scope = Scope(this, libraries, scopes.size + 1)
        scope.grow(functorNames.size)
        scopes.add(scope)
        compileInto(scope, libraries)
        return scope
    }

    private fun compileInto(
        scope: Scope,
        libraries: Runtime,
    ) {
        for (library in libraries.libraries) for (clause in library.clauses) scope.add(clause)
        scope.codeEnd = codeTop
        scope.sources = libraries.libraries.filter { it.clauses.iterator().hasNext() }
    }

    // The scope of a base program for [libraries]: the one compiled for the same aliases, if its libraries with
    // clauses are the same objects, else one compiled now (under the lock of the shared programs: a base program is
    // read by every solver derived from it)
    private fun sharedScopeOf(libraries: Runtime): Scope? =
        locked(SharedPrograms) {
            val key = libraries.libraries.map { it.alias }
            val known = sharedScopes[key]
            val sources = libraries.libraries.filter { it.clauses.iterator().hasNext() }
            if (known != null) {
                if (known.sources.size == sources.size && known.sources.indices.all { known.sources[it] === sources[it] }) known else null
            } else {
                val scope = Scope(this, libraries, sharedScopes.size + 1)
                scope.grow(functorNames.size)
                // (registered first: the functors its clauses intern grow it with the others)
                sharedScopes[key] = scope
                compileInto(scope, libraries)
                scope
            }
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

/**
 * The base programs of the solvers, one per set of libraries (by their aliases): the clauses of the libraries are
 * compiled once into it, and every solver with those libraries starts from a copy of it ([derived]), so a solver
 * compiles its own theory alone
 */
internal object SharedPrograms {
    private val bases = HashMap<List<String>, Program>()

    /** A program for a solver with [libraries], derived from their base program */
    fun derived(libraries: Runtime): Program =
        locked(this) {
            val base = bases.getOrPut(libraries.libraries.map { it.alias }) { Program() }
            Program(base)
        }
}
