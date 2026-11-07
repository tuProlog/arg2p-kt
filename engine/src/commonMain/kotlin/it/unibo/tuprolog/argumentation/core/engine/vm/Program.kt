package it.unibo.tuprolog.argumentation.core.engine.vm

import it.unibo.tuprolog.argumentation.core.engine.vm.Words.ATOM
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.BIG
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.FLT
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.FUN
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.INT
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.LIST
import it.unibo.tuprolog.argumentation.core.runtime.context.WordTable
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Real
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.theory.Theory
import org.gciatto.kt.math.BigInteger
import kotlin.jvm.JvmField

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
    var functorArities = IntArray(1 shl 10)
        private set

    // The functor an atom was last used in and its arity plus one, by atom
    private var lastFunctorOf = IntArray(1 shl 10)
    private var lastArityOf = IntArray(1 shl 10)

    // The functors asked for by name lately
    private val recentNames = arrayOfNulls<String>(RECENT)
    private val recentArities = IntArray(RECENT)
    private val recentFunctors = IntArray(RECENT)
    private val functorIds = HashMap<Int, Int>()

    // The symbols of the base program when this one was made, shared by the programs of the solvers as they are never changed:
    // putting them in a map for each solver hashed all the names of all the modules
    private var baseAtomIds: Map<String, Int>? = null
    private var baseFunctorIds: Map<Int, Int>? = null
    private var sharedAtoms: Map<String, Int>? = null
    private var sharedFunctors: Map<Int, Int>? = null

    private fun atomsToShare(): Map<String, Int> =
        sharedAtoms?.takeIf { it.size == atomIds.size } ?: HashMap(atomIds).also { sharedAtoms = it }

    private fun functorsToShare(): Map<Int, Int> =
        sharedFunctors?.takeIf { it.size == functorIds.size } ?: HashMap(functorIds).also { sharedFunctors = it }

    private val wideFunctorIds = HashMap<Long, Int>()

    // (the floats as tuProlog reads them: a float read from "61.167" and the double 61.167 are two values, and the
    // first must come out as it went in)
    val floats = ArrayList<Real>()
    private val floatIds = HashMap<Real, Int>()
    val bigs = ArrayList<Long>()

    /** The integers beyond 64 bits: a large integer word with a negative payload, -1 - index, stands for one */
    val huges = ArrayList<BigInteger>()
    private val hugeIds = HashMap<BigInteger, Int>()
    private val bigIds = HashMap<Long, Int>()

    /** The code of the clauses: instructions and their operands */
    @JvmField
    var code = IntArray(1 shl 14)
    var codeTop = 0

    /** The highest register any clause uses */
    var maxRegister = 0

    // The program this one was derived from, if any (see SharedPrograms): its symbols and its code up to
    // [baseCodeTop] are this program's too, so the scopes it compiled by then serve here as they are
    private var base: Program? = null
    private var baseCodeTop = 0

    // The code of the libraries: of a base program by the source of the module, of any other by the library
    private val sharedCodes = HashMap<CodeKey, LibraryCode>()

    // The scope last asked for by its libraries
    private var latestScope: Scope? = null
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
        baseAtomIds = base.atomsToShare()
        functorNames.clear()
        functorNames.addAll(base.functorNames)
        functorArities = base.functorArities.copyOf()
        functorIds.clear()
        baseFunctorIds = base.functorsToShare()
        wideFunctorIds.putAll(base.wideFunctorIds)
        floats.clear()
        floats.addAll(base.floats)
        floatIds.clear()
        floatIds.putAll(base.floatIds)
        bigs.clear()
        bigs.addAll(base.bigs)
        huges.addAll(base.huges)
        hugeIds.putAll(base.hugeIds)
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

    // The addresses of the blocks of control every machine of this program runs (see Machine)
    private var controlBlocks: IntArray? = null

    fun controlBlocks(make: () -> IntArray): IntArray = controlBlocks ?: make().also { controlBlocks = it }

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
        /** The keys of the arguments of its first argument, if that is compound and any is keyed; empty otherwise */
        val innerKeys: IntArray = NO_KEYS,
        /** Whether it leaves its arguments in the registers up to a cut opening it, its head and tests changing no register */
        val guarded: Boolean = false,
        /**
         * Whether its head cannot fail, a neck cut following, once the first key matched and the arguments of this mask,
         * as bits, are unbound; -1 if it may. No alternative is then ever reached
         */
        val commitMask: Int = -1,
    ) {
        val key: Int get() = if (keys.isEmpty()) 0 else keys[0]

        /** Whether an argument beyond the first is keyed */
        val deep: Boolean get() = innerKeys.isNotEmpty() || keys.size > 1 && (1 until keys.size).any { keys[it] != 0 }
    }

    /**
     * The clauses of a predicate at some point, flat for the selection to read: once [pinned], a call still running on them
     * may come back to them, so they do not change, and that call sees them as they were when it was made
     */
    class ClauseTable(
        val capacity: Int,
    ) {
        val clauses = arrayOfNulls<ClauseCode>(capacity)
        val firstKeys = IntArray(capacity)
        val starts = IntArray(capacity)
        var size = 0

        /** Whether a clause keys an argument beyond the first, or within it */
        var deep = false

        /** Whether a clause keys the arguments within its first argument */
        var innerDeep = false
        var pinned = false

        fun copied(toCapacity: Int): ClauseTable {
            val copy = ClauseTable(toCapacity)
            clauses.copyInto(copy.clauses, 0, 0, size)
            firstKeys.copyInto(copy.firstKeys, 0, 0, size)
            starts.copyInto(copy.starts, 0, 0, size)
            copy.size = size
            copy.deep = deep
            copy.innerDeep = innerDeep
            return copy
        }
    }

    /** The clauses of a predicate, and which of them a first argument may match */
    class Predicate(
        val functor: Int,
    ) {
        /** The clauses now: what a choice point holds is never changed, the next change makes another (see [ClauseTable.pinned]) */
        var table = ClauseTable(4)
            private set

        /** The clauses, as a list */
        val clauses: List<ClauseCode> get() = table.let { t -> List(t.size) { t.clauses[it]!! } }

        private var all: IntArray? = null
        private var byKey: WordTable<IntArray>? = null

        // The clauses with no key, for the keys a frozen predicate does not have
        private var frozenMiss: IntArray? = null

        /** Whether a clause keys an argument beyond the first */
        val deep: Boolean get() = table.deep

        /** How many clauses there are */
        val size: Int get() = table.size

        fun add(
            clause: ClauseCode,
            onTop: Boolean,
        ) {
            val current = table
            val n = current.size
            val updated =
                if (current.pinned ||
                    n == current.capacity
                ) {
                    current.copied(maxOf(4, if (n == current.capacity) n * 2 else current.capacity))
                } else {
                    current
                }
            val at = if (onTop) 0 else n
            if (onTop) {
                updated.clauses.copyInto(updated.clauses, 1, 0, n)
                updated.firstKeys.copyInto(updated.firstKeys, 1, 0, n)
                updated.starts.copyInto(updated.starts, 1, 0, n)
            }
            updated.clauses[at] = clause
            updated.firstKeys[at] = clause.key
            updated.starts[at] = clause.start
            updated.size = n + 1
            if (clause.deep) updated.deep = true
            if (clause.innerKeys.isNotEmpty()) updated.innerDeep = true
            table = updated
            changed()
        }

        fun removeAt(index: Int) {
            val current = table
            val n = current.size
            val updated = if (current.pinned) current.copied(current.capacity) else current
            updated.clauses.copyInto(updated.clauses, index, index + 1, n)
            updated.firstKeys.copyInto(updated.firstKeys, index, index + 1, n)
            updated.starts.copyInto(updated.starts, index, index + 1, n)
            updated.clauses[n - 1] = null
            updated.size = n - 1
            updated.deep = false
            updated.innerDeep = false
            for (i in 0 until n - 1) {
                val clause = updated.clauses[i]!!
                if (clause.deep) updated.deep = true
                if (clause.innerKeys.isNotEmpty()) updated.innerDeep = true
            }
            table = updated
            changed()
        }

        private fun changed() {
            all = null
            byKey = null
            deepIndexes = null
            deepAsks = 0
        }

        // The indexes of the arguments beyond the first, by the position of the argument: each made once goals keep
        // asking for it with the clauses unchanged (replaced as a whole, for the threads reading a frozen predicate)
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
            val current = table
            val n = current.size
            val groups = HashMap<Int, ArrayList<Int>>()
            val miss = ArrayList<Int>()
            for (i in 0 until n) {
                val keys = current.clauses[i]!!.keys
                val key = if (position < keys.size) keys[position] else 0
                if (key == 0) miss.add(i) else groups.getOrPut(key) { ArrayList() }.add(i)
            }
            // (clauses with no key are candidates for every key: the lists they lengthen are bounded, or the scan is as good)
            val worthIt = groups.size >= 2 && (miss.size <= DEEP_INDEX_MISSES || miss.size * groups.size <= n * DEEP_INDEX_MISSES)
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
            val current = table
            if (key == 0 || current.size < INDEXED_FROM) return all ?: IntArray(current.size) { it }.also { all = it }
            val miss = frozenMiss
            if (miss != null) return byKey!![key] ?: miss
            val index = byKey ?: WordTable<IntArray>().also { byKey = it }
            return index.getOrPut(key) {
                val matching = ArrayList<Int>()
                for (i in 0 until current.size) {
                    val k = current.firstKeys[i]
                    if (k == 0 || k == key) matching.add(i)
                }
                matching.toIntArray()
            }
        }

        /** Everything [candidates] may need computed now: the predicate is then only read, by any thread */
        fun freeze() {
            val current = table
            val keys = current.firstKeys
            val n = current.size
            current.pinned = true
            all = IntArray(n) { it }
            if (n < INDEXED_FROM) return
            val index = WordTable<IntArray>()
            for (i in 0 until n) {
                val key = keys[i]
                if (key != 0 && index[key] == null) {
                    index[key] = (0 until n).filter { keys[it] == 0 || keys[it] == key }.toIntArray()
                }
            }
            byKey = index
            frozenMiss = (0 until n).filter { keys[it] == 0 }.toIntArray()
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

        /** See NativeCall.libraryValue */
        var libraryValue: Any? = null
        var libraryValueKnown = false

        private var primitives = arrayOfNulls<Primitive>(0)
        private var natives = arrayOfNulls<StdlibNative>(0)
        private var looked = BooleanArray(0)

        fun predicate(functor: Int): Predicate? = if (functor < predicates.size) predicates[functor] else null

        /** The primitive of the libraries for [functor], if any */
        fun primitive(functor: Int): Primitive? {
            val libraries = libraries ?: return null
            if (functor >= primitives.size) {
                val size = maxOf(primitives.size * 2, program.functorNames.size, functor + 1)
                primitives = primitives.copyOf(size)
                natives = natives.copyOf(size)
                looked = looked.copyOf(size)
            }
            if (!looked[functor]) {
                looked[functor] = true
                val signature = Signature(program.nameOfFunctor(functor), program.functorArities[functor])
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
        baseAtomIds?.get(name) ?: atomIds.getOrPut(name) {
            atoms.add(name)
            atoms.size - 1
        }

    fun atomWord(name: String): Int = word(ATOM, atom(name))

    fun functor(
        name: String,
        arity: Int,
    ): Int {
        // (the primitives ask by name for the few functors they build, over and over)
        val at = (name.hashCode() + arity) and (RECENT - 1)
        val known = recentNames[at]
        if (known != null && recentArities[at] == arity && (known === name || known == name)) return recentFunctors[at]
        val functor = functor(atom(name), arity)
        recentNames[at] = name
        recentArities[at] = arity
        recentFunctors[at] = functor
        return functor
    }

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
        // (an atom is used with one arity for the most part: that one with no hash, no box)
        if (nameAtom < lastFunctorOf.size && lastArityOf[nameAtom] == arity + 1) return lastFunctorOf[nameAtom]
        val functor = functorOfAtom(nameAtom, arity)
        if (nameAtom >= lastFunctorOf.size) {
            val size = maxOf(nameAtom + 1, lastFunctorOf.size * 2)
            lastFunctorOf = lastFunctorOf.copyOf(size)
            lastArityOf = lastArityOf.copyOf(size)
        }
        lastFunctorOf[nameAtom] = functor
        lastArityOf[nameAtom] = arity + 1
        return functor
    }

    private fun functorOfAtom(
        nameAtom: Int,
        arity: Int,
    ): Int {
        // (an Int for most, with no Long: a BigInt on JS, asked for by each functor not seen last)
        val makeNew = {
            functorNames.add(nameAtom)
            val id = functorNames.size - 1
            if (id == functorArities.size) functorArities = functorArities.copyOf(id * 2)
            functorArities[id] = arity
            val functors = functorNames.size
            theory.grow(functors)
            for (scope in scopes) scope.grow(functors)
            id
        }
        return if (nameAtom < 1 shl 21 && arity < 1 shl 10) {
            val key = (nameAtom shl 10) or arity
            baseFunctorIds?.get(key) ?: functorIds.getOrPut(key, makeNew)
        } else {
            wideFunctorIds.getOrPut((nameAtom.toLong() shl 32) or arity.toLong(), makeNew)
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

    // The terms of the atoms, and of the small integers from 0: read for each answer and each argument of a primitive, and immutable
    private var atomTerms = arrayOfNulls<Atom>(256)
    private val integerTerms = arrayOfNulls<Integer>(256)

    /** The term of the atom of id [id] */
    fun atomTerm(id: Int): Atom {
        if (id >= atomTerms.size) atomTerms = atomTerms.copyOf(maxOf(id + 1, atomTerms.size * 2))
        return atomTerms[id] ?: Atom.of(atoms[id]).also { atomTerms[id] = it }
    }

    /** The term of the integer [value] */
    fun integerTerm(value: Int): Integer {
        if (value < 0 || value >= integerTerms.size) return Integer.of(value)
        return integerTerms[value] ?: Integer.of(value).also { integerTerms[value] = it }
    }

    /** The word of the integer [value], with no Long unless it takes one (a Long is a BigInt on JS) */
    fun smallWord(value: Int): Int = if (value >= Words.INT_MIN && value <= Words.INT_MAX) word(INT, value) else bigWord(value.toLong())

    /** The word of the sum of two Ints, with no Long unless it takes one */
    fun sumWord(
        a: Int,
        b: Int,
    ): Int {
        val sum = a + b
        return if (sum >= Words.INT_MIN && sum <= Words.INT_MAX && (a xor sum) and (b xor sum) >= 0) {
            word(INT, sum)
        } else {
            bigWord(a.toLong() + b)
        }
    }

    /** The word of the integer [value], of any size */
    fun integerWord(value: BigInteger): Int {
        if (value.bitLength <= 28) return word(INT, value.toInt())
        val small = value.toLong()
        if (BigInteger.of(small) == value) return intWord(small)
        val index =
            hugeIds.getOrPut(value) {
                huges.add(value)
                huges.size - 1
            }
        return word(BIG, -1 - index)
    }

    /** The value of a large integer word of [payload] */
    fun bigInteger(payload: Int): BigInteger = if (payload < 0) huges[-1 - payload] else BigInteger.of(bigs[payload])

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
        val latest = latestScope
        if (latest != null && latest.libraries === libraries) return latest
        // (with no iterator: the calls of modules alternate, and each comes here)
        var i = scopes.size - 1
        while (i >= 0) {
            val scope = scopes[i]
            if (scope.libraries === libraries) {
                latestScope = scope
                return scope
            }
            i--
        }
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

        private const val RECENT = 64

        /** No keys at all */
        val NO_KEYS = IntArray(0)

        /** The arguments of the first argument of a clause that are keyed */
        const val INNER_KEYS = 4
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
 * The key of the code of a library: its text is long, and a string is hashed anew at each lookup on JS. [of] keeps one for
 * each alias, for the text of a module to be hashed once in a process
 */
internal class CodeKey(
    private val text: String,
) {
    private val hash = text.hashCode()

    override fun hashCode(): Int = hash

    override fun equals(other: Any?): Boolean = other === this || (other is CodeKey && other.hash == hash && other.text == text)

    companion object {
        private val known = HashMap<String, Pair<String, CodeKey>>()

        fun of(
            alias: String,
            theory: String,
        ): CodeKey =
            locked(known) {
                known[alias]?.takeIf { it.first == theory }?.second
                    ?: CodeKey(alias + "\n" + theory).also { known[alias] = theory to it }
            }
    }
}

/**
 * The library of a module whose clauses are the same for every instance of it, [codeKey] telling them apart: its code
 * is compiled once per process (see [SharedPrograms]), rather than for each solver
 */

internal class SharedLibrary(
    private val library: Library,
    val codeKey: CodeKey,
) : Library by library {
    // (the hash of a library goes through all it holds, and each runtime made of it asks for it)
    private val hash by lazy { library.hashCode() }

    override fun hashCode(): Int = hash
}
