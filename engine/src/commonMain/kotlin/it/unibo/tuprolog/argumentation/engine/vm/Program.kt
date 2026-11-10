package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi
import it.unibo.tuprolog.argumentation.engine.words.Symbols
import it.unibo.tuprolog.argumentation.engine.words.Words
import it.unibo.tuprolog.argumentation.engine.words.Words.ATOM
import it.unibo.tuprolog.argumentation.engine.words.Words.BIG
import it.unibo.tuprolog.argumentation.engine.words.Words.FLT
import it.unibo.tuprolog.argumentation.engine.words.Words.FUN
import it.unibo.tuprolog.argumentation.engine.words.Words.INT
import it.unibo.tuprolog.argumentation.engine.words.locked
import it.unibo.tuprolog.argumentation.engine.words.payload
import it.unibo.tuprolog.argumentation.engine.words.word
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Real
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.theory.Theory
import kotlin.jvm.JvmField

/**
 * A program of the machine: the symbol tables (atoms, functors, floats and large integers, each once, so that
 * equal constants are equal words), the code array the clauses are compiled into (see [Compiler]) and the
 * predicates: those of the theory of the user ([theory]) and, for each set of libraries a resolution runs with, those
 * of the clauses of the libraries and the primitives they provide (a [Scope]).
 */
@InternalArg2pApi
class Program(
    codeSize: Int = 1 shl 14,
    /** The symbols of the program: a table of its own, or one the programs of a session share (see [Symbols]) */
    val symbols: Symbols = Symbols.of(null),
) {
    // The arities as the symbols last had them, read by each unification of a compound: the array is the symbols' own (they
    // only add to it, or move to a larger one, read again when an id is past its end)
    @JvmField
    var arities: IntArray = symbols.functorArities

    // The functor an atom was last used in and its arity plus one, by atom
    private var lastFunctorOf = IntArray(1 shl 10)
    private var lastArityOf = IntArray(1 shl 10)

    // The functors asked for by name lately
    private val recentNames = arrayOfNulls<String>(RECENT)
    private val recentArities = IntArray(RECENT)
    private val recentFunctors = IntArray(RECENT)

    // The functors the tables of the predicates were grown for (see grown)
    private var grownFor = 0

    /** The code of the clauses: instructions and their operands */
    @JvmField
    var code = IntArray(codeSize)
    var codeTop = 0

    /** The highest register any clause uses */
    var maxRegister = 0

    // The program this one was derived from, if any (see SharedPrograms): its symbols and its code up to
    // [baseCodeTop] are this program's too, so the scopes it compiled by then serve here as they are
    private var base: Program? = null
    private var baseCodeTop = 0

    // The code of the libraries: of a base program by the source of the module, of any other by the library
    private val sharedCodes = HashMap<CodeKey, LibraryCode>()

    // The tables of the predicates of the scopes of the programs of this base, by the code of their libraries (the codes
    // compare as the same objects): made once, read only (under the lock of the base programs)
    private val sharedTables = HashMap<List<LibraryCode>, Array<Predicate?>>()

    // The scope last asked for by its libraries
    private var latestScope: Scope? = null
    private val localCodes = HashMap<IdentityKey, LibraryCode>()

    /** How many times the clauses of the theory of the user changed since the program was made */
    var theoryVersion = 0L

    /** The session of the program, if any: the solvers of a session share their symbols (see [SolverSession]) */
    var session: SolverSession? = null

    /**
     * Whether the words of [other] are words of this program as they are: those of the program itself, or of a program
     * sharing its symbols (another of its session)
     */
    fun readsWordsOf(other: Program?): Boolean = other === this || (other != null && other.symbols === symbols && symbols.isShared)

    /**
     * A program starting as a copy of [base]: its code (the modules compiled once, for every solver) and its symbols, in
     * a table of its own or in [symbols] (the table a session shares, and a program derived from the base of a session
     * shares too)
     */
    constructor(
        base: Program,
        symbols: Symbols = base.symbols.forDerived(),
    ) : this(codeSize = 0, symbols = symbols) {
        // (the symbols as the base has them: those the primary constructor asked for are among them)
        code = base.code.copyOf(maxOf(base.codeTop + (1 shl 12), 1 shl 14))
        codeTop = base.codeTop
        maxRegister = base.maxRegister
        this.base = base
        baseCodeTop = base.codeTop
        theory.grow(symbols.functorNames.size)
    }

    internal val compiler = Compiler(this)

    // What every machine of this program runs: its builtins by functor, and its blocks of control (see ControlBlocks)
    private var builtins: Array<Machine.Builtin?>? = null
    private var controlBlocks: ControlBlocks? = null

    internal fun builtins(make: () -> Array<Machine.Builtin?>): Array<Machine.Builtin?> = builtins ?: make().also { builtins = it }

    internal fun controlBlocks(): ControlBlocks = controlBlocks ?: ControlBlocks.of(this).also { controlBlocks = it }

    /** The predicates of the theory of the user (its static and dynamic clauses) */
    internal val theory = Scope(this, null, 0)

    /** The scopes of the libraries, by id (see [scopeOf]) */
    internal val scopes = ArrayList<Scope>()

    val emptyList: Int = atomWord("[]")

    /** The word of `true`, and the functor word of `:-/2` */
    val trueWord: Int = atomWord("true")

    /** The name of a list cell */
    val dotWord: Int = atomWord(".")
    val ruleFunctor: Int = functorWord(":-", 2)

    // The names of the compounds standing for the numbers no word holds (see Numbers), by atom id
    private val intNameId: Int = payload(atomWord(Numbers.INT_NAME))
    private val floatNameId: Int = payload(atomWord(Numbers.FLOAT_NAME))

    // The floats read last (see Numbers): a float read again, as a constant is, makes no new term. A few, each replacing the
    // one before it in its slot: a loop over floats keeps none
    private class ReadFloat(
        val key: Long,
        val real: Real,
    )

    // (made at the first float read: most theories have none)
    private var readFloats: Array<ReadFloat?>? = null

    /** The float of a scale, a sign and chunks (see [Numbers]) */
    fun realOf(
        scale: Int,
        sign: Int,
        chunks: IntArray,
    ): Real {
        if (chunks.size > 2 || scale < 0 || scale > 0x7FFF) return Numbers.realOf(scale, sign, chunks)
        val unscaled = if (chunks.size == 2) (chunks[0].toLong() shl Numbers.CHUNK_BITS) or chunks[1].toLong() else chunks[0].toLong()
        val key = (unscaled shl 16) or (scale.toLong() shl 1) or (if (sign < 0) 1L else 0L)
        val slot = ((key * -0x61c8864680b583ebL) ushr (64 - READ_FLOATS_BITS)).toInt()
        val read = readFloats ?: arrayOfNulls<ReadFloat>(READ_FLOATS).also { readFloats = it }
        read[slot]?.let { if (it.key == key) return it.real }
        return Numbers.realOf(scale, sign, chunks).also { read[slot] = ReadFloat(key, it) }
    }

    /** [BIG] for the functor word of an integer no word holds, [FLT] for that of a float (see [Numbers]), 0 for any other */
    fun numberTag(functorWord: Int): Int =
        when (symbols.functorNames[payload(functorWord)]) {
            intNameId -> BIG
            floatNameId -> FLT
            else -> 0
        }

    fun atom(name: String): Int = symbols.atom(name)

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
        val functor = symbols.functor(nameAtom, arity)
        grown()
        return functor
    }

    // The tables of the predicates as large as the functors there are (another program of a session may have added some)
    private fun grown() {
        val functors = symbols.functorNames.size
        if (functors <= grownFor) return
        grownFor = functors
        theory.grow(functors)
    }

    fun functorWord(
        name: String,
        arity: Int,
    ): Int = word(FUN, functor(name, arity))

    fun arity(functorWord: Int): Int {
        val id = payload(functorWord)
        val known = arities
        return if (id < known.size) known[id] else symbols.functorArities.also { arities = it }[id]
    }

    fun nameOfFunctorWord(functorWord: Int): String = nameOfFunctorId(payload(functorWord))

    fun nameOfFunctorId(functor: Int): String = symbols.atoms[symbols.functorNames[functor]]

    /** The word of a constant: an atom or a small integer (the other numbers are compounds, see [Numbers]) */
    fun constantWord(term: Term): Int =
        when {
            term is Atom -> atomWord(term.value)
            term is Integer && !Numbers.needsBlock(term) -> word(INT, term.value.toInt())
            else -> throw IllegalArgumentException("not a constant of the machine: $term")
        }

    // The terms of the atoms, and of the small integers from 0: read for each answer and each argument of a primitive, and immutable
    private var atomTerms = arrayOfNulls<Atom>(256)
    private val integerTerms = arrayOfNulls<Integer>(256)

    /** The term of the atom of id [id] */
    fun atomTerm(id: Int): Atom {
        if (id >= atomTerms.size) atomTerms = atomTerms.copyOf(maxOf(id + 1, atomTerms.size * 2))
        return atomTerms[id] ?: Atom.of(symbols.atoms[id]).also { atomTerms[id] = it }
    }

    /** The term of the integer [value] */
    fun integerTerm(value: Int): Integer {
        if (value < 0 || value >= integerTerms.size) return Integer.of(value)
        return integerTerms[value] ?: Integer.of(value).also { integerTerms[value] = it }
    }

    /** The word of the small integer [value] (see [Words.fitsInt]); the others are compounds on the heap (see [Numbers]) */
    fun smallWord(value: Int): Int {
        require(value >= Words.INT_MIN && value <= Words.INT_MAX) { "not a small integer: $value" }
        return word(INT, value)
    }

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
    internal fun scopeOf(libraries: Runtime): Scope {
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
        scopes.add(scope)
        scope.predicates = tableOf(libraries)
        return scope
    }

    // The predicates of [libraries] by functor (see Scope.tableOf): when the code of each is the base's, the table the base
    // keeps for that code, the same in every program of the base; this program's own otherwise
    private fun tableOf(libraries: Runtime): Array<Predicate?> {
        val codes = ArrayList<LibraryCode>()
        var ofBase = true
        for (library in libraries.libraries) {
            val code = codeOf(library) ?: continue
            codes.add(code)
            if (!code.frozen) ofBase = false
        }
        val base = base
        if (!ofBase || base == null) return Scope.tableOf(codes, frozen = false)
        return locked(SharedPrograms) { base.sharedTables.getOrPut(codes) { Scope.tableOf(codes, frozen = true) } }
    }

    private fun codeOf(library: Library): LibraryCode? {
        if (library.clauses.none()) return null
        val base = base
        if (library is SharedLibrary && base != null) {
            val shared = locked(SharedPrograms) { base.sharedCode(library) }
            if (shared.codeEnd <= baseCodeTop) return shared
        }
        return localCodes.getOrPut(IdentityKey(library)) { compiled(library, frozen = false) }
    }

    // The code of a module in a base program (under the lock of the base programs): that its own base compiled before
    // this program was made, if it has a base (the base of a session), or compiled here
    private fun sharedCode(library: SharedLibrary): LibraryCode =
        sharedCodes.getOrPut(library.codeKey) {
            base?.sharedCodes?.get(library.codeKey)?.takeIf { it.codeEnd <= baseCodeTop } ?: compiled(library, frozen = true)
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
        return LibraryCode(predicates.values.toList(), codeTop, frozen)
    }

    private class IdentityKey(
        val library: Library,
    ) {
        override fun equals(other: Any?): Boolean = other is IdentityKey && other.library === library

        override fun hashCode(): Int = library.alias.hashCode()
    }

    /** The code of [libraries] compiled now, as a resolution with them would */
    @InternalArg2pApi
    fun compile(libraries: Runtime) {
        scopeOf(libraries)
    }

    /** The scope of [id] (see [Scope.id]): the theory of the user for 0 */
    internal fun scope(id: Int): Scope = if (id == 0) theory else scopes[id - 1]

    /** The code of [query] as a clause whose head holds its variables */
    internal fun compileQuery(query: Struct): Compiler.Query = compiler.compileQuery(query)

    companion object {
        private const val RECENT = 64
        private const val READ_FLOATS_BITS = 10
        private const val READ_FLOATS = 1 shl READ_FLOATS_BITS

        /** No keys at all */
        val NO_KEYS = IntArray(0)

        /** The arguments of the first argument of a clause that are keyed */
        const val INNER_KEYS = 4
    }
}
