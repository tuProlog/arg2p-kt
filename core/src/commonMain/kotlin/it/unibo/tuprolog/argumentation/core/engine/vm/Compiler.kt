package it.unibo.tuprolog.argumentation.core.engine.vm

import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Cons
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Real
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var

/**
 * The compiler of the machine (spike): a clause into the instructions of [Op], in the code array of the [Program].
 *
 * The variables of a clause are temporary (a register, when they occur in one chunk alone: the head and the first
 * body goal, or a single other body goal) or permanent (a slot of the environment frame, when they span several
 * body goals). A variable occurring once is void. A clause has an environment when it has permanent variables, more
 * than one body goal, a cut or a control construct. Conjunctions, disjunctions, if-then-else and negation as
 * failure compile inline (`TRY_ELSE`, `GET_LEVEL`, `CUT_Y`); a variable goal becomes `call/1`. The last body goal
 * of a clause is executed in its place (last-call optimisation).
 */
internal class Compiler(
    private val program: Program,
) {
    /** A compiled query: its first instruction, and its variables in the order of the registers they are given */
    class Query(
        val start: Int,
        val variables: List<Var>,
    )

    private sealed class Body {
        class Goal(
            val term: Struct,
        ) : Body()

        class Conj(
            val items: List<Body>,
        ) : Body()

        class Disj(
            val term: Struct,
            val left: Body,
            val right: Body,
        ) : Body()

        class IfThenElse(
            val term: Struct,
            val condition: Body,
            val then: Body,
            val otherwise: Body,
        ) : Body()

        class Naf(
            val term: Struct,
            val goal: Body,
        ) : Body()

        object Cut : Body()

        object True : Body()
    }

    private class VarInfo {
        val chunks = HashSet<Int>()
        var occurrences = 0
        var permanent = false
        var register = -1
        var seen = false
        var firstInRegion = false
        var everMet = false
    }

    private val vars = LinkedHashMap<Var, VarInfo>()
    private var chunk = 0
    private var leafGoals = 0
    private var hasControl = false
    private var hasCut = false
    private var levels = 0
    private var maxArity = 0
    private var nextTemp = 0
    private var nextLevel = 0
    private var needsEnv = false

    // The levels (frame slots) of the negations and conditions being compiled: a cut inside one is local to it
    private val cutLevels = ArrayList<Int>()

    /** Compiles [clause]: the address of its code and its first-argument key */
    fun compile(
        clause: Clause,
        dynamic: Boolean,
    ): Program.ClauseCode {
        val head = clause.head!!
        val start = compile(head, clause.body)
        val keys = IntArray(head.arity) { keyOf(head[it]) }
        return Program.ClauseCode(keys, start, clause, dynamic)
    }

    /** Compiles [query] as the body of a clause whose head holds its variables, in order */
    fun compileQuery(query: Struct): Query {
        val variables = LinkedHashSet<Var>().also { collectVariables(query, it) }.toList()
        val head = if (variables.isEmpty()) Atom.of("\$query") else Struct.of("\$query", variables)
        val start = compile(head, query)
        return Query(start, variables)
    }

    private fun keyOf(term: Term): Int =
        when (term) {
            is Var -> 0
            is Atom -> constant(term)
            is Cons -> Program.LIST_KEY
            is Struct -> program.functorWord(term.functor, term.arity)
            else -> constant(term)
        }

    private fun constant(term: Term): Int =
        when (term) {
            is Integer -> program.intWord(term.value.toLong())
            is Real -> program.floatWord(term)
            is Atom -> program.atomWord(term.value)
            else -> throw IllegalArgumentException("not a term of the machine: $term")
        }

    private fun compile(
        head: Struct,
        bodyTerm: Term,
    ): Int {
        vars.clear()
        cutLevels.clear()
        chunk = 0
        leafGoals = 0
        hasControl = false
        hasCut = false
        levels = 0
        maxArity = head.arity
        val body = parse(bodyTerm)
        // Analysis: the chunks each variable occurs in
        for (i in 0 until head.arity) occurrences(head[i], 0, region = false)
        analyze(body, first = true, region = false)
        var permanents = 0
        for (info in vars.values) {
            info.permanent = info.chunks.size > 1
            if (info.permanent) info.register = permanents++
        }
        needsEnv = permanents > 0 || leafGoals > 1 || hasControl || hasCut
        nextTemp = maxArity
        nextLevel = permanents
        val start = program.codeTop
        if (needsEnv) program.emit(Op.ALLOCATE, permanents + levels)
        for ((variable, info) in vars) {
            if (info.permanent && info.firstInRegion) {
                program.emit(Op.INIT_Y, info.register, program.nameOperand(variable))
                info.seen = true
            }
        }
        for (i in 0 until head.arity) getArgument(head[i], i)
        val executed = emit(body, last = true)
        if (!executed) {
            if (needsEnv) program.emit(Op.DEALLOCATE)
            program.emit(Op.PROCEED)
        }
        if (nextTemp > program.maxRegister) program.maxRegister = nextTemp
        return start
    }

    // ---- analysis

    private fun parse(term: Term): Body =
        when {
            term.isTrue -> Body.True
            term is Var -> Body.Goal(Struct.of("call", term))
            term is Atom && term.value == "!" -> Body.Cut
            term is Struct && term.arity == 2 && term.functor == "," -> {
                val items = ArrayList<Body>()
                var current: Term = term
                while (current is Struct && current.arity == 2 && current.functor == ",") {
                    items.add(parse(current[0]))
                    current = current[1]
                }
                items.add(parse(current))
                Body.Conj(items)
            }
            term is Struct && term.arity == 2 && term.functor == ";" -> {
                val left = term[0]
                if (left is Struct && left.arity == 2 && left.functor == "->") {
                    Body.IfThenElse(term, parse(left[0]), parse(left[1]), parse(term[1]))
                } else {
                    Body.Disj(term, parse(left), parse(term[1]))
                }
            }
            term is Struct && term.arity == 2 && term.functor == "->" -> {
                Body.IfThenElse(term, parse(term[0]), parse(term[1]), parse(Atom.of("fail")))
            }
            term is Struct && term.arity == 1 && term.functor == "\\+" -> Body.Naf(term, parse(term[0]))
            term is Struct -> Body.Goal(term)
            // (a number as a goal: call/1 raises the type error when the clause runs)
            else -> Body.Goal(Struct.of("call", term))
        }

    private fun analyze(
        body: Body,
        first: Boolean,
        region: Boolean,
    ) {
        when (body) {
            is Body.Goal -> {
                val goal = body.term
                leafGoals++
                if (goal.arity > maxArity) maxArity = goal.arity
                val c = if (first && !region) 0 else ++chunk
                for (i in 0 until goal.arity) occurrences(goal[i], c, region)
            }
            is Body.Conj -> for ((i, item) in body.items.withIndex()) analyze(item, first && i == 0, region)
            is Body.Disj ->
                if (definedByTheory(body.term)) {
                    analyze(Body.Goal(body.term), first, region)
                } else {
                    hasControl = true
                    analyze(body.left, first = false, region = true)
                    analyze(body.right, first = false, region = true)
                }
            is Body.IfThenElse ->
                if (definedByTheory(body.term)) {
                    analyze(Body.Goal(body.term), first, region)
                } else {
                    hasControl = true
                    levels++
                    analyze(body.condition, first = false, region = true)
                    analyze(body.then, first = false, region = true)
                    analyze(body.otherwise, first = false, region = true)
                }
            is Body.Naf ->
                if (definedByTheory(body.term)) {
                    analyze(Body.Goal(body.term), first, region)
                } else {
                    hasControl = true
                    levels++
                    analyze(body.goal, first = false, region = true)
                }
            Body.Cut -> hasCut = true
            Body.True -> Unit
        }
    }

    // Whether the theory of the user has clauses for the functor of [term], a control construct: it is then called
    // as a goal, so that the engine's reading holds (the clauses of a library predicate followed by the theory's:
    // the strict rules of an argumentation theory are clauses of ->/2). Told when the clause is compiled.
    private fun definedByTheory(term: Struct): Boolean = program.theory.predicate(program.functor(term.functor, term.arity)) != null

    private fun occurrences(
        term: Term,
        chunk: Int,
        region: Boolean,
    ) {
        when (term) {
            is Var -> {
                val info = vars.getOrPut(term) { VarInfo() }
                info.chunks.add(chunk)
                info.occurrences++
                if (!info.everMet) {
                    info.everMet = true
                    info.firstInRegion = region
                }
            }
            is Struct -> for (i in 0 until term.arity) occurrences(term[i], chunk, region)
            else -> Unit
        }
    }

    private fun collectVariables(
        term: Term,
        into: LinkedHashSet<Var>,
    ) {
        when (term) {
            is Var -> into.add(term)
            is Struct -> for (i in 0 until term.arity) collectVariables(term[i], into)
            else -> Unit
        }
    }

    // ---- emission

    /** Emits [body]; true if it ended by executing a goal in place (the clause then needs no return) */
    private fun emit(
        body: Body,
        last: Boolean,
    ): Boolean {
        when (body) {
            is Body.Goal -> {
                val goal = body.term
                for (i in 0 until goal.arity) putArgument(goal[i], i)
                val functor = program.functor(goal.functor, goal.arity)
                if (last) {
                    if (needsEnv) program.emit(Op.DEALLOCATE)
                    program.emit(Op.EXECUTE, functor)
                    return true
                }
                program.emit(Op.CALL, functor)
                return false
            }
            is Body.Conj -> {
                val items = body.items
                for ((i, item) in items.withIndex()) {
                    if (emit(item, last && i == items.size - 1)) return true
                }
                return false
            }
            is Body.Disj -> {
                if (definedByTheory(body.term)) return emit(Body.Goal(body.term), last)
                program.emit(Op.TRY_ELSE)
                val elseLabel = program.emit(0)
                emit(body.left, last = false)
                program.emit(Op.JUMP)
                val endLabel = program.emit(0)
                program.code[elseLabel] = program.codeTop
                program.emit(Op.TRUST_ME)
                emit(body.right, last = false)
                program.code[endLabel] = program.codeTop
                return false
            }
            is Body.IfThenElse -> {
                if (definedByTheory(body.term)) return emit(Body.Goal(body.term), last)
                val level = nextLevel++
                program.emit(Op.GET_LEVEL, level)
                program.emit(Op.TRY_ELSE)
                val elseLabel = program.emit(0)
                cutLevels.add(level)
                emit(body.condition, last = false)
                cutLevels.removeAt(cutLevels.size - 1)
                program.emit(Op.CUT_Y, level)
                emit(body.then, last = false)
                program.emit(Op.JUMP)
                val endLabel = program.emit(0)
                program.code[elseLabel] = program.codeTop
                program.emit(Op.TRUST_ME)
                emit(body.otherwise, last = false)
                program.code[endLabel] = program.codeTop
                return false
            }
            is Body.Naf -> {
                if (definedByTheory(body.term)) return emit(Body.Goal(body.term), last)
                val level = nextLevel++
                program.emit(Op.GET_LEVEL, level)
                program.emit(Op.TRY_ELSE)
                val elseLabel = program.emit(0)
                cutLevels.add(level)
                emit(body.goal, last = false)
                cutLevels.removeAt(cutLevels.size - 1)
                program.emit(Op.CUT_Y, level)
                program.emit(Op.FAIL)
                program.code[elseLabel] = program.codeTop
                program.emit(Op.TRUST_ME)
                return false
            }
            Body.Cut -> {
                // (a cut inside a negation or a condition is local to it: the choice point of the construct stays)
                if (cutLevels.isEmpty()) program.emit(Op.CUT) else program.emit(Op.CUT_LOCAL, cutLevels[cutLevels.size - 1])
                return false
            }
            Body.True -> return false
        }
    }

    private fun getArgument(
        term: Term,
        register: Int,
    ) {
        when (term) {
            is Var -> {
                val info = vars[term]!!
                if (info.occurrences == 1) return
                if (!info.seen) {
                    info.seen = true
                    if (info.permanent) {
                        program.emit(Op.GET_Y_VAR, info.register, register)
                    } else {
                        info.register = nextTemp++
                        program.emit(Op.GET_X_VAR, info.register, register)
                    }
                } else {
                    program.emit(if (info.permanent) Op.GET_Y_VAL else Op.GET_X_VAL, info.register, register)
                }
            }
            is Atom -> program.emit(Op.GET_CONST, constant(term), register)
            is Cons -> {
                program.emit(Op.GET_LIST, register)
                unifyArguments(listOf(term.head, term.tail))
            }
            is Struct -> {
                program.emit(Op.GET_STR, program.functorWord(term.functor, term.arity), register)
                unifyArguments(List(term.arity) { term[it] })
            }
            else -> program.emit(Op.GET_CONST, constant(term), register)
        }
    }

    // The arguments of a structure being unified, the compound ones through temporaries unified after it
    private fun unifyArguments(arguments: List<Term>) {
        val pending = ArrayList<Pair<Int, Term>>()
        var voids = 0

        fun flushVoids() {
            if (voids > 0) {
                program.emit(Op.UNIFY_VOID, voids)
                voids = 0
            }
        }
        for (argument in arguments) {
            when (argument) {
                is Var -> {
                    val info = vars[argument]!!
                    if (info.occurrences == 1) {
                        voids++
                        continue
                    }
                    flushVoids()
                    if (!info.seen) {
                        info.seen = true
                        if (info.permanent) {
                            program.emit(Op.UNIFY_Y_VAR, info.register, program.nameOperand(argument))
                        } else {
                            info.register = nextTemp++
                            program.emit(Op.UNIFY_X_VAR, info.register, program.nameOperand(argument))
                        }
                    } else {
                        program.emit(if (info.permanent) Op.UNIFY_Y_VAL else Op.UNIFY_X_VAL, info.register)
                    }
                }
                is Atom -> {
                    flushVoids()
                    program.emit(Op.UNIFY_CONST, constant(argument))
                }
                is Struct -> {
                    flushVoids()
                    val temp = nextTemp++
                    program.emit(Op.UNIFY_X_VAR, temp, 0)
                    pending.add(temp to argument)
                }
                else -> {
                    flushVoids()
                    program.emit(Op.UNIFY_CONST, constant(argument))
                }
            }
        }
        flushVoids()
        for ((temp, argument) in pending) getArgument(argument, temp)
    }

    private fun putArgument(
        term: Term,
        register: Int,
    ) {
        when (term) {
            is Var -> {
                val info = vars[term]!!
                if (!info.seen) {
                    info.seen = true
                    if (info.permanent) {
                        program.emit(Op.PUT_Y_VAR, info.register, register, program.nameOperand(term))
                    } else {
                        info.register = nextTemp++
                        program.emit(Op.PUT_X_VAR, info.register, register, program.nameOperand(term))
                    }
                } else {
                    program.emit(if (info.permanent) Op.PUT_Y_VAL else Op.PUT_X_VAL, info.register, register)
                }
            }
            is Atom -> program.emit(Op.PUT_CONST, constant(term), register)
            is Struct -> {
                val arguments = if (term is Cons) listOf(term.head, term.tail) else List(term.arity) { term[it] }
                // The compound arguments built first, into temporaries
                val built = HashMap<Int, Int>()
                for ((i, argument) in arguments.withIndex()) {
                    if (argument is Struct && argument !is Atom) {
                        val temp = nextTemp++
                        putArgument(argument, temp)
                        built[i] = temp
                    }
                }
                if (term is Cons) {
                    program.emit(Op.PUT_LIST, register)
                } else {
                    program.emit(Op.PUT_STR, program.functorWord(term.functor, term.arity), register)
                }
                var voids = 0
                for ((i, argument) in arguments.withIndex()) {
                    when (argument) {
                        is Var -> {
                            val info = vars[argument]!!
                            if (info.occurrences == 1) {
                                voids++
                                continue
                            }
                            if (voids > 0) {
                                program.emit(Op.SET_VOID, voids)
                                voids = 0
                            }
                            if (!info.seen) {
                                info.seen = true
                                if (info.permanent) {
                                    program.emit(Op.SET_Y_VAR, info.register, program.nameOperand(argument))
                                } else {
                                    info.register = nextTemp++
                                    program.emit(Op.SET_X_VAR, info.register, program.nameOperand(argument))
                                }
                            } else {
                                program.emit(if (info.permanent) Op.SET_Y_VAL else Op.SET_X_VAL, info.register)
                            }
                        }
                        is Atom -> {
                            if (voids > 0) {
                                program.emit(Op.SET_VOID, voids)
                                voids = 0
                            }
                            program.emit(Op.SET_CONST, constant(argument))
                        }
                        is Struct -> {
                            if (voids > 0) {
                                program.emit(Op.SET_VOID, voids)
                                voids = 0
                            }
                            program.emit(Op.SET_X_VAL, built[i]!!)
                        }
                        else -> {
                            if (voids > 0) {
                                program.emit(Op.SET_VOID, voids)
                                voids = 0
                            }
                            program.emit(Op.SET_CONST, constant(argument))
                        }
                    }
                }
                if (voids > 0) program.emit(Op.SET_VOID, voids)
            }
            else -> program.emit(Op.PUT_CONST, constant(term), register)
        }
    }

    // ---- the blocks of the control constructs called as goals at run time (see Machine)

    /** The code of `','(A, B)` with the goals in registers 0 and 1 */
    fun conjunctionBlock(): Int {
        val start = program.codeTop
        program.emit(Op.ALLOCATE, 2)
        program.emit(Op.GET_Y_VAR, 0, 0)
        program.emit(Op.GET_Y_VAR, 1, 1)
        program.emit(Op.CALL_TERM, 0, Op.TRANSPARENT)
        program.emit(Op.PUT_Y_VAL, 1, 0)
        program.emit(Op.DEALLOCATE)
        program.emit(Op.EXECUTE_TERM, 0, Op.TRANSPARENT)
        return start
    }

    /** The code of `';'(A, B)` (not an if-then-else) with the goals in registers 0 and 1 */
    fun disjunctionBlock(): Int {
        val start = program.codeTop
        program.emit(Op.ALLOCATE, 2)
        program.emit(Op.GET_Y_VAR, 0, 0)
        program.emit(Op.GET_Y_VAR, 1, 1)
        program.emit(Op.TRY_ELSE)
        val elseLabel = program.emit(0)
        program.emit(Op.CALL_TERM, 0, Op.TRANSPARENT)
        program.emit(Op.DEALLOCATE)
        program.emit(Op.PROCEED)
        program.code[elseLabel] = program.codeTop
        program.emit(Op.TRUST_ME)
        program.emit(Op.PUT_Y_VAL, 1, 0)
        program.emit(Op.DEALLOCATE)
        program.emit(Op.EXECUTE_TERM, 0, Op.TRANSPARENT)
        return start
    }

    /** The code of `(C -> T ; E)` with the goals in registers 0, 1 and 2 */
    fun ifThenElseBlock(): Int {
        val start = program.codeTop
        program.emit(Op.ALLOCATE, 4)
        program.emit(Op.GET_Y_VAR, 0, 0)
        program.emit(Op.GET_Y_VAR, 1, 1)
        program.emit(Op.GET_Y_VAR, 2, 2)
        program.emit(Op.GET_LEVEL, 3)
        program.emit(Op.TRY_ELSE)
        val elseLabel = program.emit(0)
        program.emit(Op.CALL_TERM, 0, Op.OPAQUE)
        program.emit(Op.CUT_Y, 3)
        program.emit(Op.PUT_Y_VAL, 1, 0)
        program.emit(Op.DEALLOCATE)
        program.emit(Op.EXECUTE_TERM, 0, Op.TRANSPARENT)
        program.code[elseLabel] = program.codeTop
        program.emit(Op.TRUST_ME)
        program.emit(Op.PUT_Y_VAL, 2, 0)
        program.emit(Op.DEALLOCATE)
        program.emit(Op.EXECUTE_TERM, 0, Op.TRANSPARENT)
        return start
    }

    /** The code of `\+ G` with the goal in register 0 */
    fun negationBlock(): Int {
        val start = program.codeTop
        program.emit(Op.ALLOCATE, 2)
        program.emit(Op.GET_Y_VAR, 0, 0)
        program.emit(Op.GET_LEVEL, 1)
        program.emit(Op.TRY_ELSE)
        val elseLabel = program.emit(0)
        program.emit(Op.CALL_TERM, 0, Op.OPAQUE)
        program.emit(Op.CUT_Y, 1)
        program.emit(Op.FAIL)
        program.code[elseLabel] = program.codeTop
        program.emit(Op.TRUST_ME)
        program.emit(Op.DEALLOCATE)
        program.emit(Op.PROCEED)
        return start
    }

    /**
     * The code an inlined goal returns to: the scope of the caller restored from the frame the inlining allocated
     * (see Machine.inline), the frame deallocated
     */
    fun restoreScopeBlock(): Int {
        val start = program.codeTop
        program.emit(Op.RESTORE_SCOPE)
        program.emit(Op.DEALLOCATE)
        program.emit(Op.PROCEED)
        return start
    }

    /** The address of a `STOP` instruction: the continuation of a run that ends with a solution */
    fun stopBlock(): Int {
        val start = program.codeTop
        program.emit(Op.STOP)
        return start
    }
}
