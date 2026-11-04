package it.unibo.tuprolog.argumentation.core.engine.vm

import it.unibo.tuprolog.argumentation.core.engine.vm.Words.ATOM
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.BIG
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.FLT
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.FUN
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.INT
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.LIST
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.payload
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.word
import it.unibo.tuprolog.argumentation.core.runtime.context.WordTable
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Real
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.theory.Theory
import kotlin.concurrent.Volatile

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

    /** The clauses compiled and the entry of each, by the instructions standing for them (see [Op.COMPILED]) */
    var compiledUnits = arrayOfNulls<CompiledCode>(64)
    var compiledEntries = IntArray(64)
    private var compiledCount = 0

    /** The clauses of [predicate] compiled, those that can be and are not yet; those of a base program for every program derived from it */
    fun compileHot(predicate: Predicate) {
        val base = base
        for (clause in predicate.clauses) {
            if (code[clause.start] >= Op.COMPILED || clause.notGenerated) continue
            val generated =
                if (base != null && clause.start < baseCodeTop) {
                    locked(SharedPrograms) { base.generatedFor(clause) }
                } else {
                    generatedFor(clause)
                }
            if (generated != null) install(generated)
        }
    }

    private fun generatedFor(clause: ClauseCode): GeneratedClause? {
        clause.generated?.let { return it }
        val generated = ClauseCodegen.generate(code, clause.start)
        if (generated == null) {
            clause.notGenerated = true
            return null
        }
        clause.generated = generated
        install(generated)
        return generated
    }

    private fun install(generated: GeneratedClause) {
        for ((entry, address) in generated.entries.withIndex()) {
            if (code[address] >= Op.COMPILED) continue
            if (compiledCount == compiledUnits.size) {
                compiledUnits = compiledUnits.copyOf(compiledCount * 2)
                compiledEntries = compiledEntries.copyOf(compiledCount * 2)
            }
            compiledUnits[compiledCount] = generated.unit
            compiledEntries[compiledCount] = entry
            code[address] = Op.COMPILED + compiledCount
            compiledCount++
        }
    }

    // The program this one was derived from, if any (see SharedPrograms): its symbols and its code up to
    // [baseCodeTop] are this program's too, so the scopes it compiled by then serve here as they are
    private var base: Program? = null
    private var baseCodeTop = 0

    // The code of the libraries: of a base program by the source of the module, of any other by the library
    private val sharedCodes = HashMap<String, LibraryCode>()
    private val localCodes = HashMap<IdentityKey, LibraryCode>()

    /** How many times the clauses of the theory of the user changed since the program was made */
    var theoryVersion = 0L
        private set

    /** A program starting as a copy of [base]: its symbols and its code (the modules compiled once, for every solver) */
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
        compiledUnits = base.compiledUnits.copyOf()
        compiledEntries = base.compiledEntries.copyOf()
        compiledCount = base.compiledCount
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
        /** The clause compiled, once it was, for all the programs sharing it */
        var generated: GeneratedClause? = null

        /** Whether compiling it was tried and cannot be done */
        var notGenerated = false

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
        private var byKey: WordTable<IntArray>? = null

        // The clauses with no key, for the keys a frozen predicate does not have
        private var frozenMiss: IntArray? = null

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

        /** How many times it was called: from a number of them on, it is compiled */
        var heat = 0

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
            deepIndexes = null
            deepAsks = 0
            version++
        }

        // The indexes of the arguments beyond the first, by the position of the argument: each made once goals keep
        // asking for it with the clauses unchanged (replaced as a whole, for the threads reading a frozen predicate)
        @Volatile
        private var deepIndexes: Array<DeepIndex?>? = null
        private var deepAsks = 0

        /** The clauses a goal may match by the key of an argument beyond the first; no table: it would not help */
        private class DeepIndex(
            val byKey: WordTable<IntArray>?,
            val miss: IntArray,
        )

        /**
         * The indexes of the clauses a goal with [key] in its argument [position] (beyond the first) may match, in
         * order; null if no index of that argument says
         */
        fun candidatesAt(
            position: Int,
            key: Int,
        ): IntArray? {
            val index = deepIndexes?.get(position)
            if (index == null) {
                if (++deepAsks < DEEP_INDEX_AFTER) return null
                return locked(this) { deepIndex(position) }.let { made -> made.byKey?.let { it[key] ?: made.miss } }
            }
            val table = index.byKey ?: return null
            return table[key] ?: index.miss
        }

        private fun deepIndex(position: Int): DeepIndex {
            deepIndexes?.get(position)?.let { return it }
            val n = clauses.size
            val groups = HashMap<Int, ArrayList<Int>>()
            val miss = ArrayList<Int>()
            for (i in 0 until n) {
                val keys = clauses[i].keys
                val key = if (position < keys.size) keys[position] else 0
                if (key == 0) miss.add(i) else groups.getOrPut(key) { ArrayList() }.add(i)
            }
            // (clauses with no key are candidates for every key: with more than a few, the index is not worth its lists)
            val worthIt = groups.size >= 2 && miss.size <= DEEP_INDEX_MISSES
            val table = if (worthIt) WordTable<IntArray>() else null
            if (table != null) {
                for ((key, group) in groups) {
                    table[key] = (if (miss.isEmpty()) group else (group + miss).sorted()).toIntArray()
                }
            }
            val index = DeepIndex(table, miss.toIntArray())
            val updated = deepIndexes?.copyOf() ?: arrayOfNulls<DeepIndex>(MAX_DEEP_POSITION + 1)
            updated[position] = index
            deepIndexes = updated
            return index
        }

        /**
         * The indexes of the clauses a goal whose first argument has [key] may match, in order: all of them for an
         * unbound argument or a small predicate, otherwise those with the same key or none
         */
        fun candidates(key: Int): IntArray {
            if (key == 0 || clauses.size < INDEXED_FROM) return all ?: IntArray(clauses.size) { it }.also { all = it }
            val miss = frozenMiss
            if (miss != null) return byKey!![key] ?: miss
            val index = byKey ?: WordTable<IntArray>().also { byKey = it }
            return index.getOrPut(key) {
                val matching = ArrayList<Int>()
                for (i in clauses.indices) {
                    val k = clauses[i].key
                    if (k == 0 || k == key) matching.add(i)
                }
                matching.toIntArray()
            }
        }

        /** Everything [candidates] may need computed now: the predicate is then only read, by any thread */
        fun freeze() {
            val n = clauses.size
            all = IntArray(n) { it }
            if (n < INDEXED_FROM) return
            val index = WordTable<IntArray>()
            for (clause in clauses) {
                val key = clause.key
                if (key != 0 && index[key] == null) {
                    index[key] = (0 until n).filter { clauses[it].key == 0 || clauses[it].key == key }.toIntArray()
                }
            }
            byKey = index
            frozenMiss = (0 until n).filter { clauses[it].key == 0 }.toIntArray()
        }

        companion object {
            const val INDEXED_FROM = 8

            /** The last argument an index is made for */
            const val MAX_DEEP_POSITION = 3

            private const val DEEP_INDEX_AFTER = 16
            private const val DEEP_INDEX_MISSES = 8
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
        private var natives = arrayOfNulls<StdlibNative>(0)
        private var looked = BooleanArray(0)

        fun predicate(functor: Int): Predicate? = if (functor < predicates.size) predicates[functor] else null

        /** The primitive of the libraries for [functor], if any */
        fun primitive(functor: Int): Primitive? {
            val libraries = libraries ?: return null
            if (functor >= primitives.size) {
                val size = maxOf(program.functorNames.size, functor + 1)
                primitives = primitives.copyOf(size)
                natives = natives.copyOf(size)
                looked = looked.copyOf(size)
            }
            if (!looked[functor]) {
                looked[functor] = true
                val signature = Signature(program.nameOfFunctor(functor), program.functorArities[functor])
                primitives[functor] = libraries.primitives[signature]
                natives[functor] = StdlibNatives.of(signature, primitives[functor])
            }
            return primitives[functor]
        }

        /** The native standing in for the primitive of [functor], looked up by [primitive] */
        fun native(functor: Int): StdlibNative? = natives[functor]

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
            if (this === program.theory) program.theoryVersion++
        }

        /** The predicates of a library added to the scope: shared as they are, those it has already joined in a new one */
        internal fun merge(code: LibraryCode) {
            grow(program.functorNames.size)
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

    val emptyList: Int = atomWord("[]")

    /** The word of `true`, and the functor word of `:-/2` */
    val trueWord: Int = atomWord("true")

    /** The name of a list cell */
    val dotWord: Int = atomWord(".")
    val ruleFunctor: Int = functorWord(":-", 2)

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

    // The functors of arity 0 by the id of their atom, plus one: an atom called as a goal looks its predicate up by them
    private var atomFunctors = IntArray(256)

    /** The functor of the atom [nameAtom] as a goal */
    fun atomFunctor(nameAtom: Int): Int {
        if (nameAtom < atomFunctors.size) {
            val known = atomFunctors[nameAtom]
            if (known != 0) return known - 1
        } else {
            atomFunctors = atomFunctors.copyOf(maxOf(nameAtom + 1, atomFunctors.size * 2))
        }
        val functor = functor(nameAtom, 0)
        atomFunctors[nameAtom] = functor + 1
        return functor
    }

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
    fun add(theory: Theory) = add(theory.clauses)

    /** Adds [clauses] to those of the theory of the user */
    fun add(clauses: Iterable<Clause>) {
        compiler.definedAlongside(clauses)
        for (clause in clauses) theory.add(clause)
        compiler.definedAlongside(emptyList())
    }

    /**
     * The scope of [libraries], made when first asked for: the code of each library, compiled once for this program,
     * or taken from the base program for a module it compiled before this program was made
     */
    fun scopeOf(libraries: Runtime): Scope {
        for (scope in scopes) if (scope.libraries === libraries) return scope
        val scope = Scope(this, libraries, scopes.size + 1)
        scope.grow(functorNames.size)
        scopes.add(scope)
        for (library in libraries.libraries) codeOf(library)?.let { scope.merge(it) }
        return scope
    }

    private fun codeOf(library: Library): LibraryCode? {
        if (library.clauses.none()) return null
        val base = base
        if (library is SharedLibrary && base != null) {
            val shared = locked(SharedPrograms) { base.sharedCodes.getOrPut(library.codeKey) { base.compiled(library, frozen = true) } }
            if (shared.codeEnd <= baseCodeTop) return shared
        }
        return localCodes.getOrPut(IdentityKey(library)) { compiled(library, frozen = false) }
    }

    // The clauses of [library] compiled into this program, by predicate (frozen: for other programs to read at once)
    private fun compiled(
        library: Library,
        frozen: Boolean,
    ): LibraryCode {
        val predicates = LinkedHashMap<Int, Predicate>()
        compiler.definedAlongside(library.clauses)
        for (clause in library.clauses) {
            val head = clause.head ?: continue
            val functor = functor(head.functor, head.arity)
            val code = compiler.compile(clause, false)
            predicates.getOrPut(functor) { Predicate(functor) }.add(code, false)
        }
        compiler.definedAlongside(emptyList())
        if (frozen) predicates.values.forEach { it.freeze() }
        return LibraryCode(predicates.values.toList(), codeTop)
    }

    /** The code of a library: its predicates, and where the code of its clauses ends */
    internal class LibraryCode(
        val predicates: List<Predicate>,
        val codeEnd: Int,
    )

    private class IdentityKey(
        val library: Library,
    ) {
        override fun equals(other: Any?): Boolean = other is IdentityKey && other.library === library

        override fun hashCode(): Int = library.alias.hashCode()
    }

    /** The scope of [id] (see [Scope.id]): the theory of the user for 0 */
    fun scope(id: Int): Scope = if (id == 0) theory else scopes[id - 1]

    /** The code of [query] as a clause whose head holds its variables */
    fun compileQuery(query: Struct): Compiler.Query = compiler.compileQuery(query)

    companion object {
        /** The first-argument key of a list cell */
        const val LIST_KEY = (0 shl Words.TAG_BITS) or LIST

        /** The calls of a predicate, a power of 2, that have its clauses compiled: fewer and a short solve pays for code still cold */
        const val HOT_CALLS = 1 shl 14
    }
}

/**
 * The base program of the solvers: the clauses of each module (see [SharedLibrary]) are compiled once into it, when a
 * solver first asks for them, and every solver starts from a copy of it, compiling the rest alone. It grows with the
 * modules there are, by their source, whatever the solvers, their flags and their theories: it keeps no solver.
 */
internal object SharedPrograms {
    private val base = Program()

    /** A program for a solver */
    fun derived(): Program = locked(this) { Program(base) }

    /** The words of code of the base program */
    val codeSize: Int get() = locked(this) { base.codeTop }
}

/**
 * The library of a module whose clauses are the same for every instance of it, [codeKey] telling them apart: its code
 * is compiled once per process (see [SharedPrograms]), rather than for each solver
 */
internal class SharedLibrary(
    private val library: Library,
    val codeKey: String,
) : Library by library
