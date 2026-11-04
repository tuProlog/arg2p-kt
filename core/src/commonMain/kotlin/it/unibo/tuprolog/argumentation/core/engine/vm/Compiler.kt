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

        // The argument register a temporary variable lives in, when one is its (see compile)
        var preferred = -1
        var seen = false
        var firstInRegion = false
        var everMet = false
    }

    private val vars = LinkedHashMap<Var, VarInfo>()
    private var chunk = 0
    private var leafGoals = 0
    private var hasControl = false
    private var cutAfterGoal = false
    private var levels = 0
    private var maxArity = 0
    private var nextTemp = 0
    private var nextLevel = 0
    private var needsEnv = false

    // The goal of the first chunk (the one the head's temporaries may reach), if any
    private var firstGoal: Struct? = null

    // Whether the goal analysed last was arithmetic compiled inline: the next call joins its chunk (no call between)
    private var inlineOpen = false

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
        firstGoal = null
        inlineOpen = false
        chunk = 0
        leafGoals = 0
        hasControl = false
        cutAfterGoal = false
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
        needsEnv = permanents > 0 || leafGoals > 1 || hasControl || cutAfterGoal
        preferRegisters(head)
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
        if (!executed) program.emit(if (needsEnv) Op.RETURN else Op.PROCEED)
        if (nextTemp > program.maxRegister) program.maxRegister = nextTemp
        return start
    }

    // ---- analysis

    /**
     * The argument registers the temporaries of the head and of the first goal live in, sparing the moves: a
     * variable that is the argument i of the head and of the first goal alike stays in Ai (no instruction either
     * side); one first met within the structure of the argument i of the head, or first met in the body, that is
     * the argument k of the first goal is unified (or created) straight into Ak when the head is done with Ak
     * (k <= i: the arguments before i are read by then, Ai itself by the instruction entering the structure)
     */
    private fun preferRegisters(head: Struct) {
        val goal = firstGoal ?: return
        val firstIn = HashMap<Var, Int>()
        val bare = HashSet<Var>()
        for (i in 0 until head.arity) {
            val argument = head[i]
            if (argument is Var) {
                if (argument !in firstIn) {
                    firstIn[argument] = i
                    bare.add(argument)
                }
            } else {
                val within = LinkedHashSet<Var>().also { collectVariables(argument, it) }
                for (variable in within) if (variable !in firstIn) firstIn[variable] = i
            }
        }
        for (k in 0 until goal.arity) {
            val variable = goal[k] as? Var ?: continue
            val info = vars[variable]!!
            if (info.permanent || info.occurrences == 1 || info.preferred != -1) continue
            val i = firstIn[variable]
            when {
                i == null -> info.preferred = k
                variable in bare -> if (i == k) info.preferred = k
                k <= i -> info.preferred = k
            }
        }
    }

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
                val inline = inlineArithmetic(goal) || inlineBuiltin(goal)
                val c =
                    if (first && !region) {
                        0
                    } else if (inlineOpen) {
                        chunk
                    } else {
                        ++chunk
                    }
                if (!inline) {
                    leafGoals++
                    if (goal.arity > maxArity) maxArity = goal.arity
                    if (c == 0 && firstGoal == null) firstGoal = goal
                }
                inlineOpen = inline
                for (i in 0 until goal.arity) occurrences(goal[i], c, region)
            }
            is Body.Conj -> for ((i, item) in body.items.withIndex()) analyze(item, first && i == 0, region)
            is Body.Disj ->
                if (definedByTheory(body.term)) {
                    analyze(Body.Goal(body.term), first, region)
                } else {
                    hasControl = true
                    inlineOpen = false
                    analyze(body.left, first = false, region = true)
                    inlineOpen = false
                    analyze(body.right, first = false, region = true)
                    inlineOpen = false
                }
            is Body.IfThenElse ->
                if (definedByTheory(body.term)) {
                    analyze(Body.Goal(body.term), first, region)
                } else {
                    hasControl = true
                    levels++
                    inlineOpen = false
                    analyze(body.condition, first = false, region = true)
                    inlineOpen = false
                    analyze(body.then, first = false, region = true)
                    inlineOpen = false
                    analyze(body.otherwise, first = false, region = true)
                    inlineOpen = false
                }
            is Body.Naf ->
                if (definedByTheory(body.term)) {
                    analyze(Body.Goal(body.term), first, region)
                } else {
                    hasControl = true
                    levels++
                    inlineOpen = false
                    analyze(body.goal, first = false, region = true)
                    inlineOpen = false
                }
            Body.Cut -> if (leafGoals > 0) cutAfterGoal = true
            Body.True -> Unit
        }
    }

    // ---- arithmetic compiled inline

    /**
     * Whether [goal] is `is/2` or a comparison the theory does not redefine, over expressions of variables, small
     * integers and the integer operations the machine evaluates: compiled inline (no call: the goal joins the chunk
     * of the call after it)
     */
    private fun inlineArithmetic(goal: Struct): Boolean {
        if (goal.arity != 2 || definedByTheory(goal)) return false
        val kind = COMPARISONS[goal.functor]
        if (goal.functor == "is") {
            val target = goal[0]
            return (target is Var || (target is Integer && smallInteger(target))) && expression(goal[1])
        }
        return kind != null && expression(goal[0]) && expression(goal[1])
    }

    // The predicates the clauses being compiled along with this one define: a builtin they define is not inlined, even in
    // a clause coming before those defining it
    private var defined: Set<Int> = emptySet()

    /** The clauses compiled from now on, until the next call, come with [clauses] */
    fun definedAlongside(clauses: Iterable<Clause>) {
        defined = clauses.mapNotNullTo(HashSet()) { clause -> clause.head?.let { program.functor(it.functor, it.arity) } }
    }

    // A builtin compiled inline, on the variables where they are: a type test on a variable, == or \\== with a variable and
    // a variable or a constant, = with a variable on a side that the other does not hold (as it is a goal otherwise)
    private fun inlineBuiltin(goal: Struct): Boolean {
        val shape =
            when (goal.arity) {
                1 -> goal.functor in TYPE_TESTS && goal[0] is Var
                3 ->
                    when (goal.functor) {
                        "arg" -> goal[0].let { it is Integer && smallInteger(it) } && goal[1] is Var && simple(goal[2])
                        "functor" -> goal[0] is Var && simple(goal[1]) && simple(goal[2])
                        else -> false
                    }
                2 ->
                    when (goal.functor) {
                        "==", "\\==" -> (goal[0] is Var || goal[1] is Var) && simple(goal[0]) && simple(goal[1])
                        "=" -> unifiesInline(goal[0], goal[1]) || unifiesInline(goal[1], goal[0])
                        else -> false
                    }
                else -> false
            }
        return shape && program.functor(goal.functor, goal.arity) !in defined && !definedByTheory(goal)
    }

    private fun simple(term: Term): Boolean = term is Var || term is Atom || term !is Struct

    private fun unifiesInline(
        variable: Term,
        other: Term,
    ): Boolean = variable is Var && (other is Var || variable !in LinkedHashSet<Var>().also { collectVariables(other, it) })

    private fun emitBuiltin(goal: Struct) {
        if (goal.arity == 3) {
            val term = goal[if (goal.functor == "arg") 1 else 0] as Var
            created(term)
            if (goal.functor == "arg") {
                program.emit(Op.ARG, (goal[0] as Integer).value.toInt(), operand(term))
                emitTarget(goal[2])
            } else {
                program.emit(Op.FUNCTOR, operand(term))
                emitTarget(goal[1])
                emitTarget(goal[2])
            }
            return
        }
        if (goal.arity == 1) {
            val variable = goal[0] as Var
            created(variable)
            val info = vars[variable]!!
            program.emit(if (info.permanent) Op.TYPE_Y else Op.TYPE_X, TYPE_TESTS[goal.functor]!!, info.register)
            return
        }
        if (goal.functor == "=") {
            if (unifiesInline(goal[0], goal[1])) emitUnification(goal[0] as Var, goal[1]) else emitUnification(goal[1] as Var, goal[0])
            return
        }
        created(goal[0])
        created(goal[1])
        val negated = if (goal.functor == "==") 0 else 1
        val first = goal[0]
        val second = goal[1]
        when {
            first is Var && second is Var -> program.emit(Op.IDENTICAL, negated, operand(first), operand(second))
            first is Var -> program.emit(Op.IDENTICAL_C, negated, operand(first), constant(second))
            else -> program.emit(Op.IDENTICAL_C, negated, operand(second as Var), constant(first))
        }
    }

    // Where an inline builtin puts a value: the kind of the target and its operand
    private fun emitTarget(term: Term) {
        if (term !is Var) {
            program.emit(Op.T_CONST, constant(term))
            return
        }
        val info = vars[term]!!
        when {
            info.seen -> program.emit(if (info.permanent) Op.T_Y else Op.T_X, info.register)
            info.occurrences == 1 -> program.emit(Op.T_VOID, 0)
            info.permanent -> {
                info.seen = true
                program.emit(Op.T_NEW_Y, info.register)
            }
            else -> {
                info.seen = true
                info.register = temporary(info)
                program.emit(Op.T_NEW_X, info.register)
            }
        }
    }

    private fun operand(variable: Var): Int = vars[variable]!!.let { if (it.permanent) -1 - it.register else it.register }

    // variable = other: other unified against the variable, or built as its first value
    private fun emitUnification(
        variable: Var,
        other: Term,
    ) {
        val info = vars[variable]!!
        when {
            other == variable -> created(variable)
            info.seen -> unifyWith(info, other)
            other is Var && vars[other]!!.seen -> unifyWith(vars[other]!!, variable)
            other is Var -> {
                created(other)
                unifyWith(vars[other]!!, variable)
            }
            info.occurrences == 1 -> created(other)
            info.permanent -> {
                val temp = nextTemp++
                putArgument(other, temp)
                info.seen = true
                program.emit(Op.GET_Y_VAR, info.register, temp)
            }
            else -> {
                val register = temporary(info)
                putArgument(other, register)
                info.register = register
                info.seen = true
            }
        }
    }

    private fun unifyWith(
        info: VarInfo,
        term: Term,
    ) {
        val register =
            if (info.permanent) {
                nextTemp++.also { program.emit(Op.PUT_Y_VAL, info.register, it) }
            } else {
                info.register
            }
        getArgument(term, register)
    }

    private fun expression(term: Term): Boolean =
        when (term) {
            is Var -> true
            is Integer -> smallInteger(term)
            is Struct -> operation(term) != 0 && (0 until term.arity).all { expression(term[it]) }
            else -> false
        }

    private fun smallInteger(term: Integer): Boolean = Words.fitsInt(term.value.toLong())

    private fun operation(term: Struct): Int =
        when (term.arity) {
            2 ->
                when (term.functor) {
                    "+" -> Op.E_ADD
                    "-" -> Op.E_SUB
                    "*" -> Op.E_MUL
                    "//" -> Op.E_IDIV
                    "mod" -> Op.E_MOD
                    else -> 0
                }
            1 -> if (term.functor == "-") Op.E_NEG else 0
            else -> 0
        }

    private fun emitArithmetic(goal: Struct) {
        val functor = goal.functor
        if (functor == "is") {
            val target = goal[0]
            created(goal[1])
            program.emit(Op.ARITH_IS)
            if (target is Var) {
                val info = vars[target]!!
                if (info.seen) {
                    program.emit(if (info.permanent) Op.T_Y else Op.T_X, info.register)
                } else {
                    info.seen = true
                    if (info.permanent) {
                        program.emit(Op.T_NEW_Y, info.register)
                    } else {
                        info.register = temporary(info)
                        program.emit(Op.T_NEW_X, info.register)
                    }
                }
            } else {
                program.emit(Op.T_CONST, constant(target))
            }
            emitExpression(goal[1])
        } else {
            created(goal[0])
            created(goal[1])
            program.emit(Op.ARITH_CMP, COMPARISONS[functor]!!)
            emitExpression(goal[0])
            emitExpression(goal[1])
        }
    }

    // The variables of [term] not met before, created where they live (an instantiation error follows)
    private fun created(term: Term) {
        when (term) {
            is Var -> {
                val info = vars[term]!!
                if (info.seen) return
                info.seen = true
                if (info.permanent) {
                    program.emit(Op.INIT_Y, info.register, program.nameOperand(term))
                } else {
                    info.register = temporary(info)
                    program.emit(Op.PUT_X_VAR, info.register, info.register, program.nameOperand(term))
                }
            }
            is Struct -> for (i in 0 until term.arity) created(term[i])
            else -> Unit
        }
    }

    // The code of [term] in postfix, after its length
    private fun emitExpression(term: Term) {
        val lengthAt = program.emit(0)
        val start = program.codeTop
        emitElements(term)
        program.code[lengthAt] = program.codeTop - start
    }

    private fun emitElements(term: Term) {
        when (term) {
            is Var -> {
                val info = vars[term]!!
                program.emit(if (info.permanent) Op.E_Y else Op.E_X, info.register)
            }
            is Integer -> program.emit(Op.E_CONST, constant(term))
            is Struct -> {
                for (i in 0 until term.arity) emitElements(term[i])
                program.emit(operation(term), 0)
            }
            else -> throw IllegalStateException("not an expression: $term")
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
                if (inlineArithmetic(goal)) {
                    emitArithmetic(goal)
                    return false
                }
                if (inlineBuiltin(goal)) {
                    emitBuiltin(goal)
                    return false
                }
                for (i in 0 until goal.arity) putArgument(goal[i], i)
                val functor = program.functor(goal.functor, goal.arity)
                if (last) {
                    program.emit(if (needsEnv) Op.DEALLOCATE_EXECUTE else Op.EXECUTE, functor, goal.arity)
                    return true
                }
                program.emit(Op.CALL, functor, goal.arity)
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
                when {
                    cutLevels.isNotEmpty() -> program.emit(Op.CUT_LOCAL, cutLevels[cutLevels.size - 1])
                    needsEnv -> program.emit(Op.CUT)
                    else -> program.emit(Op.CUT_NECK)
                }
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
                        info.register = temporary(info)
                        if (info.register != register) program.emit(Op.GET_X_VAR, info.register, register)
                    }
                } else {
                    program.emit(if (info.permanent) Op.GET_Y_VAL else Op.GET_X_VAL, info.register, register)
                }
            }
            is Atom -> program.emit(Op.GET_CONST, constant(term), register)
            is Cons -> {
                val head = term.head
                val tail = term.tail
                if (head is Var && tail is Var && head != tail && freshTemporary(head) && freshTemporary(tail)) {
                    val h = vars[head]!!
                    val t = vars[tail]!!
                    h.seen = true
                    h.register = temporary(h)
                    t.seen = true
                    t.register = temporary(t)
                    program.emit(Op.GET_LIST_VV, register)
                    program.emit(h.register, program.nameOperand(head))
                    program.emit(t.register, program.nameOperand(tail))
                } else {
                    program.emit(Op.GET_LIST2, register)
                    unifyTwoArguments(head, tail)
                }
            }
            is Struct -> {
                if (term.arity == 2) {
                    program.emit(Op.GET_STR2, program.functorWord(term.functor, term.arity), register)
                    unifyTwoArguments(term[0], term[1])
                } else {
                    program.emit(Op.GET_STR, program.functorWord(term.functor, term.arity), register)
                    unifyArguments(List(term.arity) { term[it] })
                }
            }
            else -> program.emit(Op.GET_CONST, constant(term), register)
        }
    }

    private fun temporary(info: VarInfo): Int = if (info.preferred != -1) info.preferred else nextTemp++

    // A temporary variable occurring more than once, not met yet
    private fun freshTemporary(variable: Var): Boolean {
        val info = vars[variable]!!
        return !info.permanent && !info.seen && info.occurrences > 1
    }

    // The two arguments of a list or of a structure of two, as the sub-instructions of the superinstruction
    // entering it (emitted after its opcode and operands), the compound ones through temporaries unified after
    private fun unifyTwoArguments(
        first: Term,
        second: Term,
    ) {
        val pending = ArrayList<Pair<Int, Term>>()
        subInstruction(first, pending)
        subInstruction(second, pending)
        for ((temp, argument) in pending) getArgument(argument, temp)
    }

    private fun subInstruction(
        argument: Term,
        pending: ArrayList<Pair<Int, Term>>,
    ) {
        when (argument) {
            is Var -> {
                val info = vars[argument]!!
                if (info.occurrences == 1) {
                    program.emit(Op.U_VOID, 0, 0)
                } else if (!info.seen) {
                    info.seen = true
                    if (info.permanent) {
                        program.emit(Op.U_Y_VAR, info.register, program.nameOperand(argument))
                    } else {
                        info.register = temporary(info)
                        program.emit(Op.U_X_VAR, info.register, program.nameOperand(argument))
                    }
                } else {
                    program.emit(if (info.permanent) Op.U_Y_VAL else Op.U_X_VAL, info.register, 0)
                }
            }
            is Atom -> program.emit(Op.U_CONST, constant(argument), 0)
            is Struct -> {
                val temp = nextTemp++
                program.emit(Op.U_X_VAR, temp, 0)
                pending.add(temp to argument)
            }
            else -> program.emit(Op.U_CONST, constant(argument), 0)
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
                            info.register = temporary(info)
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
                        info.register = temporary(info)
                        program.emit(Op.PUT_X_VAR, info.register, register, program.nameOperand(term))
                    }
                } else if (info.permanent) {
                    program.emit(Op.PUT_Y_VAL, info.register, register)
                } else if (info.register != register) {
                    program.emit(Op.PUT_X_VAL, info.register, register)
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
                                    info.register = temporary(info)
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

    private companion object {
        fun mask(vararg tags: Int): Int = tags.fold(0) { mask, tag -> mask or (1 shl tag) }

        // The tags that pass each type test
        val TYPE_TESTS =
            mapOf(
                "var" to mask(Words.REF),
                "nonvar" to (mask(Words.REF).inv() and 0xFF),
                "atom" to mask(Words.ATOM),
                "atomic" to mask(Words.ATOM, Words.INT, Words.BIG, Words.FLT),
                "callable" to mask(Words.ATOM, Words.STR, Words.LIST),
                "compound" to mask(Words.STR, Words.LIST),
                "float" to mask(Words.FLT),
                "integer" to mask(Words.INT, Words.BIG),
                "number" to mask(Words.INT, Words.BIG, Words.FLT),
            )

        val COMPARISONS =
            mapOf(
                "is" to 0,
                "<" to Op.C_LT,
                ">" to Op.C_GT,
                "=<" to Op.C_LE,
                ">=" to Op.C_GE,
                "=:=" to Op.C_EQ,
                "=\\=" to Op.C_NE,
            )
    }

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
