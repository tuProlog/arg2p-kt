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
import org.gciatto.kt.math.BigInteger
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

    // What every machine of this program runs: its builtins by functor, and the addresses of its blocks of control (once for
    // each program: a machine for each solve would leave them behind in its code), in this order: the end of a run, then the
    // conjunction, the disjunction, the if-then-else, the negation, the return from an inlined goal (see Machine)
    private var builtins: Array<Machine.Builtin?>? = null
    private var controlBlocks: IntArray? = null

    internal fun builtins(make: () -> Array<Machine.Builtin?>): Array<Machine.Builtin?> = builtins ?: make().also { builtins = it }

    fun controlBlocks(): IntArray =
        controlBlocks ?: intArrayOf(
            compiler.stopBlock(),
            compiler.conjunctionBlock(),
            compiler.disjunctionBlock(),
            compiler.ifThenElseBlock(),
            compiler.negationBlock(),
            compiler.restoreScopeBlock(),
        ).also { controlBlocks = it }

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
        for (scope in scopes) scope.grow(functors)
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

    fun floatWord(value: Real): Int = word(FLT, symbols.float(value))

    fun bigWord(value: Long): Int = word(BIG, symbols.big(value))

    /** The word of a constant: a number or an atom */
    fun constantWord(term: Term): Int =
        when (term) {
            is Integer -> integerWord(term.value)
            is Real -> floatWord(term)
            is Atom -> atomWord(term.value)
            else -> throw IllegalArgumentException("not a term of the machine: $term")
        }

    /** The word of an integer: a small one as it is, a large one by the table */
    fun intWord(value: Long): Int = if (Words.fitsInt(value)) word(INT, value.toInt()) else bigWord(value)

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
        return word(BIG, -1 - symbols.huge(value))
    }

    /** The value of a large integer word of [payload] */
    fun bigInteger(payload: Int): BigInteger = if (payload < 0) symbols.huges[-1 - payload] else BigInteger.of(symbols.bigs[payload])

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
        scope.grow(symbols.functorNames.size)
        scopes.add(scope)
        for (library in libraries.libraries) codeOf(library)?.let { scope.merge(it) }
        return scope
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
        return LibraryCode(predicates.values.toList(), codeTop)
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

        /** No keys at all */
        val NO_KEYS = IntArray(0)

        /** The arguments of the first argument of a clause that are keyed */
        const val INNER_KEYS = 4
    }
}
