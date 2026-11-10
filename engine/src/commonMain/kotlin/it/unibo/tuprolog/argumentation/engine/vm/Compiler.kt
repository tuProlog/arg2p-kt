package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.term.collectVariables
import it.unibo.tuprolog.argumentation.engine.words.Words
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Cons
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var

/**
 * The compiler of the machine: a clause into the instructions of [Op], in the code array of the [Program].
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
        // The first chunk it occurs in, and whether it occurs in another
        var firstChunk = -1
        var inMoreChunks = false
        var occurrences = 0
        var permanent = false
        var register = -1

        // The argument register a temporary variable lives in, when one is its (see compile)
        var preferred = -1
        var seen = false
        var firstInRegion = false
        var everMet = false
    }

    private val emitter = CodeEmitter(program)
    private val vars = LinkedHashMap<Var, VarInfo>()
    private var chunk = 0
    private var leafGoals = 0
    private var hasControl = false
    private var cutAfterGoal = false

    // The permanents that are one argument of the head and of the first goal alike, by that argument: the register still
    // holds them when the goal is called (temporaries live above, the only one placed there is the goal's own)
    private val stillInArgument = HashMap<Var, Int>()
    private var placingFirstGoal = false

    // The goals compiled inline before the first goal of the clause
    private val inlineGoals = ArrayList<Struct>()
    private var levels = 0
    private var maxArity = 0
    private var nextTemp = 0
    private var nextLevel = 0

    // The goal of the first chunk (the one the head's temporaries may reach), if any
    private var firstGoal: Struct? = null

    // Whether the goal analysed last was arithmetic compiled inline: the next call joins its chunk (no call between)
    private var inlineOpen = false

    // The levels (frame slots) of the negations and conditions being compiled: a cut inside one is local to it
    private val cutLevels = ArrayList<Int>()

    /** Compiles [clause]: the address of its code and what selecting it reads */
    fun compile(
        clause: Clause,
        dynamic: Boolean,
    ): ClauseCode {
        // (the numbers no word holds compiled as the compounds they are, see Numbers; the clause kept as written)
        val head = Numbers.encoded(clause.head!!) as Struct
        val start = compile(head, Numbers.encoded(clause.body))
        val keys = IntArray(head.arity) { keyOf(head[it]) }
        return ClauseCode(
            keys,
            start,
            clause,
            dynamic,
            innerKeysOf(head),
            guardedFrom(start, head.arity),
            commitMaskOf(head, clause.body),
        )
    }

    /** Compiles [query] as the body of a clause whose head holds its variables, in order */
    fun compileQuery(written: Struct): Query {
        val query = Numbers.encoded(written) as Struct
        val variables = LinkedHashSet<Var>().also { query.collectVariables(it) }.toList()
        val head = if (variables.isEmpty()) Atom.of("\$query") else Struct.of("\$query", variables)
        val start = compile(head, query)
        return Query(start, variables)
    }

    // See ClauseCode.commitMask. A term against an unbound argument is written, which cannot fail; a variable met again
    // alone, or in the first argument, is a unification that can
    private fun commitMaskOf(
        head: Struct,
        body: Term,
    ): Int {
        val leadingCut = isCut(body) || (body is Struct && body.arity == 2 && body.functor == "," && isCut(body[0]))
        if (!leadingCut || head.arity > 31) return -1
        val met = LinkedHashSet<Var>()
        var mask = 0
        for (i in 0 until head.arity) {
            val argument = head[i]
            when {
                argument is Var -> if (!met.add(argument)) return -1
                i == 0 -> {
                    if (argument is Struct && argument !is Atom) {
                        val inner = if (argument is Cons) listOf(argument.head, argument.tail) else List(argument.arity) { argument[it] }
                        for (term in inner) if (term !is Var || !met.add(term)) return -1
                    }
                }
                else -> {
                    mask = mask or (1 shl i)
                    argument.collectVariables(met)
                }
            }
        }
        return mask
    }

    private fun isCut(term: Term): Boolean = term is Atom && term.value == "!"

    private fun innerKeysOf(head: Struct): IntArray {
        val first = (if (head.arity > 0) head[0] else null) as? Struct ?: return Program.NO_KEYS
        if (first.arity == 0) return Program.NO_KEYS
        val keys = IntArray(minOf(first.arity, Program.INNER_KEYS)) { keyOf(first[it]) }
        return if (keys.all { it == 0 }) Program.NO_KEYS else keys
    }

    // Whether the code from [start] is tests alone up to a neck cut: on arguments that are where the call left them (a
    // variable moved to a temporary leaves them so)
    private fun guardedFrom(
        start: Int,
        arity: Int,
    ): Boolean {
        val code = program.code
        var at = start
        while (true) {
            when (code[at]) {
                Op.CUT_NECK -> return at > start
                Op.GET_CONST_CUT_PROCEED -> return true
                Op.COMPARE_VV, Op.COMPARE_VC -> at += 5 + code[at + 4]
                Op.GET_CONST, Op.GET_X_VAL -> at += 3
                Op.GET_X_VAR -> if (code[at + 1] >= arity) at += 3 else return false
                Op.GET_X_VAR_TYPE_X -> if (code[at + 1] >= arity) at += 5 else return false
                Op.GET_CONST2 -> at += 5
                Op.ARITH_CMP -> at += code[at + 2].let { n -> 4 + n + code[at + 3 + n] }
                Op.TYPE_X -> at += 3
                Op.IDENTICAL, Op.IDENTICAL_C -> at += 4
                else -> return false
            }
        }
    }

    private fun keyOf(term: Term): Int =
        when (term) {
            is Var -> 0
            is Atom -> constant(term)
            is Cons -> Words.LIST_KEY
            is Struct -> program.functorWord(term.functor, term.arity)
            else -> constant(term)
        }

    private fun constant(term: Term): Int = program.constantWord(term)

    private fun compile(
        head: Struct,
        bodyTerm: Term,
    ): Int {
        vars.clear()
        cutLevels.clear()
        firstGoal = null
        stillInArgument.clear()
        placingFirstGoal = false
        inlineGoals.clear()
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
            info.permanent = info.inMoreChunks
            if (info.permanent) info.register = permanents++
        }
        emitter.needsEnv = permanents > 0 || leafGoals > 1 || hasControl || cutAfterGoal
        preferRegisters(head)
        nextTemp = maxArity
        nextLevel = permanents
        val start = program.codeTop
        emitter.openClause(permanents + levels)
        for ((variable, info) in vars) {
            if (info.permanent && info.firstInRegion) {
                program.emit(Op.INIT_Y, info.register, program.nameOperand(variable))
                info.seen = true
            }
        }
        for (i in 0 until head.arity) getArgument(head[i], i)
        val executed = emit(body, last = true)
        if (!executed) emitter.emitProceed()
        if (nextTemp > program.maxRegister) program.maxRegister = nextTemp
        return start
    }

    // ---- analysis

    /**
     * The argument registers the temporaries of the head and of the first goal live in, sparing the moves: a
     * variable that is the argument i of the head and of the first goal alike stays in Ai (no instruction either
     * side); one first met within the structure of the argument i of the head, or first met in the body, that is
     * the argument k of the first goal is unified (or created) straight into Ak when the head is done with Ak
     * (k <= i: the arguments before i are read by then, Ai itself by the instruction entering the structure). A variable
     * that is the argument i of the head and that the first goal does not hold stays in Ai too, if nothing is put there
     * before its last read: with no call in the clause, or as the first goal puts its arguments after the inline goals
     */
    private fun preferRegisters(head: Struct) {
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
                val within = LinkedHashSet<Var>().also { argument.collectVariables(it) }
                for (variable in within) if (variable !in firstIn) firstIn[variable] = i
            }
        }
        val goal = firstGoal
        if (goal == null) {
            for (i in 0 until head.arity) if (firstIn[head[i]] == i && head[i] in bare) keepInArgument(head[i], i)
            return
        }
        for (k in 0 until goal.arity) {
            val variable = goal[k] as? Var ?: continue
            val info = vars[variable]!!
            if (info.permanent &&
                variable in bare &&
                firstIn[variable] == k &&
                k < head.arity &&
                head[k] == variable
            ) {
                stillInArgument[variable] = k
            }
            if (info.permanent || info.occurrences == 1 || info.preferred != -1) continue
            val i = firstIn[variable]
            when {
                i == null -> info.preferred = k
                variable in bare -> if (i == k) info.preferred = k
                k <= i -> info.preferred = k
            }
        }
        for (i in 0 until head.arity) {
            val argument = head[i]
            if (argument !is Var || firstIn[argument] != i) continue
            val taking = if (i < goal.arity) goal[i] as? Var else null
            val placed = taking != null && vars[taking]!!.preferred == i
            if (!occursIn(argument, goal)) {
                if (!placed || (taking!! !in firstIn && overwrittenAfterLastRead(argument, taking))) keepInArgument(argument, i)
            } else if (!placed && isOneArgumentOf(argument, goal)) {
                // (taken at another position alone: the arguments are put as a parallel move, see putArguments)
                keepInArgument(argument, i)
            }
        }
    }

    // Whether [variable] occurs in [goal] once, as an argument itself
    private fun isOneArgumentOf(
        variable: Var,
        goal: Struct,
    ): Boolean {
        var count = 0
        for (k in 0 until goal.arity) {
            val argument = goal[k]
            if (argument == variable) {
                count++
            } else if (occursIn(variable, argument)) {
                return false
            }
        }
        return count == 1
    }

    private fun keepInArgument(
        term: Term,
        i: Int,
    ) {
        if (term !is Var) return
        val info = vars[term]!!
        if (!info.permanent && info.occurrences > 1 && info.preferred == -1) info.preferred = i
    }

    // Whether the variable [taking] gets its register, which [variable] lives in, no sooner than the last read of [variable]:
    // created by an `is` of the inline goals, reading it there at the latest (as it computes before it stores)
    private fun overwrittenAfterLastRead(
        variable: Var,
        taking: Var,
    ): Boolean {
        var created = false
        for (goal in inlineGoals) {
            if (created) {
                if (occursIn(variable, goal)) return false
            } else if (occursIn(taking, goal)) {
                if (goal.functor != "is" || goal[0] != taking || occursIn(taking, goal[1])) return false
                created = true
            }
        }
        return created
    }

    private fun occursIn(
        variable: Var,
        term: Term,
    ): Boolean =
        when (term) {
            is Var -> term == variable
            is Struct -> (0 until term.arity).any { occursIn(variable, term[it]) }
            else -> false
        }

    // The goal of a module call with a known module and goal, unless the clauses define the call: its arguments go in the
    // registers, no term of the goal or of the call needed
    private fun moduleGoalOf(goal: Struct): Struct? {
        if (goal.arity != 2 || (goal.functor != "::" && goal.functor != ":::")) return null
        val module = goal[0]
        val called = goal[1]
        if (module !is Atom || called !is Struct || called is Cons) return null
        return if (definedByTheory(goal) || program.functor(goal.functor, goal.arity) in defined) null else called
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
                    val called = moduleGoalOf(goal) ?: goal
                    if (called.arity > maxArity) maxArity = called.arity
                    if (c == 0 && firstGoal == null) firstGoal = called
                } else if (c == 0 && firstGoal == null) {
                    inlineGoals.add(goal)
                }
                inlineOpen = inline
                for (i in 0 until goal.arity) occurrences(goal[i], c, region)
            }
            is Body.Conj -> {
                // (a cut neither moves nor reads a register: the goal after a leading one is still in the chunk of the head)
                var leading = first
                for (item in body.items) {
                    analyze(item, leading, region)
                    if (item !is Body.Cut) leading = false
                }
            }
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
        if (goal.arity != 2 || definedByTheory(goal) || program.functor(goal.functor, goal.arity) in defined) return false
        val kind = Op.COMPARISONS[goal.functor]
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
                1 -> goal.functor in Words.TYPE_TESTS && goal[0] is Var
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
    ): Boolean = variable is Var && (other is Var || variable !in LinkedHashSet<Var>().also { other.collectVariables(it) })

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
            if (!info.permanent && emitter.lastIs(Op.GET_X_VAR)) {
                emitter.mergeInto(Op.GET_X_VAR_TYPE_X)
                program.emit(Words.TYPE_TESTS[goal.functor]!!)
                program.emit(info.register)
            } else {
                program.emit(if (info.permanent) Op.TYPE_Y else Op.TYPE_X, Words.TYPE_TESTS[goal.functor]!!, info.register)
            }
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
                emitter.emitGetYVar(info.register, temp)
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
                nextTemp++.also { emitter.emitPutYVal(info.register, it) }
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

    private fun smallInteger(term: Integer): Boolean = !Numbers.needsBlock(term)

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
            val shortcut = incrementOf(goal[1])
            val skipAt =
                if (shortcut != null) {
                    program.emit(Op.IS_VC, operand(shortcut.first), shortcut.second)
                    program.emit(0)
                } else {
                    -1
                }
            val general = program.codeTop
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
            if (skipAt >= 0) program.code[skipAt] = program.codeTop - general
        } else {
            created(goal[0])
            created(goal[1])
            val kind = Op.COMPARISONS[functor]!!
            val skipAt = emitComparisonShortcut(kind, goal[0], goal[1])
            val general = program.codeTop
            program.emit(Op.ARITH_CMP, kind)
            emitExpression(goal[0])
            emitExpression(goal[1])
            if (skipAt >= 0) program.code[skipAt] = program.codeTop - general
        }
    }

    // [expression] as the variable and the small integer it adds to it, if that is what it is
    private fun incrementOf(expression: Term): Pair<Var, Int>? {
        if (expression !is Struct || expression.arity != 2) return null
        val left = expression[0]
        val right = expression[1]
        return when {
            expression.functor == "+" && left is Var && right is Integer && smallInteger(right) -> left to right.value.toInt()
            expression.functor == "+" && right is Var && left is Integer && smallInteger(left) -> right to left.value.toInt()
            expression.functor == "-" && left is Var && right is Integer && smallInteger(right) -> left to -right.value.toInt()
            else -> null
        }
    }

    // The comparison on two variables, or a variable and a small integer, in one instruction: where the words of the general
    // one following go, -1 if it takes the general one alone
    private fun emitComparisonShortcut(
        kind: Int,
        left: Term,
        right: Term,
    ): Int {
        when {
            left is Var && right is Var -> program.emit(Op.COMPARE_VV, kind, operand(left), operand(right))
            left is Var && right is Integer && smallInteger(right) -> program.emit(Op.COMPARE_VC, kind, operand(left), right.value.toInt())
            right is Var &&
                left is Integer &&
                smallInteger(
                    left,
                ) -> program.emit(Op.COMPARE_VC, mirrored(kind), operand(right), left.value.toInt())
            else -> return -1
        }
        return program.emit(0)
    }

    private fun mirrored(kind: Int): Int =
        when (kind) {
            Op.C_LT -> Op.C_GT
            Op.C_GT -> Op.C_LT
            Op.C_LE -> Op.C_GE
            Op.C_GE -> Op.C_LE
            else -> kind
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
    // as a goal, for the clauses of a library predicate to be followed by the theory's (the strict rules of an
    // argumentation theory are clauses of ->/2). Told when the clause is compiled.
    private fun definedByTheory(term: Struct): Boolean = program.theory.predicate(program.functor(term.functor, term.arity)) != null

    private fun occurrences(
        term: Term,
        chunk: Int,
        region: Boolean,
    ) {
        when (term) {
            is Var -> {
                val info = vars.getOrPut(term) { VarInfo() }
                if (info.firstChunk == -1) {
                    info.firstChunk = chunk
                } else if (info.firstChunk != chunk) {
                    info.inMoreChunks = true
                }
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
                val moduleGoal = moduleGoalOf(goal)
                if (moduleGoal != null) {
                    putArguments(moduleGoal)
                    val kind = program.functor(goal.functor, goal.arity)
                    val called = program.functor(moduleGoal.functor, moduleGoal.arity)
                    if (last) {
                        program.emit(Op.MODULE_EXECUTE, kind, constant(goal[0]), called)
                        program.emit(moduleGoal.arity)
                        program.emit(if (emitter.needsEnv) 1 else 0)
                    } else {
                        program.emit(Op.MODULE_CALL, kind, constant(goal[0]), called)
                        program.emit(moduleGoal.arity)
                    }
                    return last
                }
                val functor = program.functor(goal.functor, goal.arity)
                val isArg = goal.functor == "arg" && goal.arity == 3 && functor !in defined && !definedByTheory(goal)
                if (isArg && !last && argToSlot(goal, functor)) return false
                placingFirstGoal = goal === firstGoal
                putArguments(goal)
                if (placingFirstGoal) stillInArgument.clear()
                placingFirstGoal = false
                if (isArg) program.emit(Op.ARG_CALL)
                emitter.emitCall(last, functor, goal.arity)
                return last
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
                val endLabel = emitBranchEnd(emit(body.left, last), last)
                emitter.placeLabel(elseLabel)
                program.emit(Op.TRUST_ME)
                return emitLastBranch(emit(body.right, last), last, endLabel)
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
                val endLabel = emitBranchEnd(emit(body.then, last), last)
                emitter.placeLabel(elseLabel)
                program.emit(Op.TRUST_ME)
                return emitLastBranch(emit(body.otherwise, last), last, endLabel)
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
                emitter.placeLabel(elseLabel)
                program.emit(Op.TRUST_ME)
                return false
            }
            Body.Cut -> {
                // (a cut inside a negation or a condition is local to it: the choice point of the construct stays)
                when {
                    cutLevels.isNotEmpty() -> program.emit(Op.CUT_LOCAL, cutLevels[cutLevels.size - 1])
                    else -> emitter.emitCut()
                }
                return false
            }
            Body.True -> return false
        }
    }

    // After a branch of a control construct: as the clause is over with it, if it comes last, it returns (unless it ended
    // by a call in place); otherwise a jump past the other branch, whose label is given
    private fun emitBranchEnd(
        executed: Boolean,
        last: Boolean,
    ): Int {
        if (last) {
            if (!executed) emitter.emitProceed()
            return -1
        }
        program.emit(Op.JUMP)
        return program.emit(0)
    }

    private fun emitLastBranch(
        executed: Boolean,
        last: Boolean,
        endLabel: Int,
    ): Boolean {
        if (last) {
            if (!executed) emitter.emitProceed()
            return true
        }
        emitter.placeLabel(endLabel)
        return false
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
                        emitter.emitGetYVar(info.register, register)
                    } else {
                        info.register = temporary(info)
                        if (info.register != register) emitter.emitMerging(Op.GET_X_VAR, info.register, register)
                    }
                } else {
                    if (info.permanent) {
                        program.emit(Op.GET_Y_VAL, info.register, register)
                    } else {
                        emitter.emitGetXVal(info.register, register)
                    }
                }
            }
            is Atom -> emitter.emitGetConst(constant(term), register)
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
                    if (emitter.lastIs(Op.GET_LIST2)) {
                        emitter.mergeInto(Op.GET_LIST2_GET_LIST2)
                        program.emit(register)
                        unifyTwoArguments(head, tail)
                    } else {
                        val at = program.codeTop
                        program.emit(Op.GET_LIST2, register)
                        unifyTwoArguments(head, tail, at)
                    }
                }
            }
            is Struct -> {
                if (term.arity == 2) {
                    val at = program.codeTop
                    val following = emitter.lastIs(Op.GET_STR2)
                    if (following) emitter.rewriteLast(Op.GET_STR2_GET_STR2)
                    program.emit(Op.GET_STR2, program.functorWord(term.functor, term.arity), register)
                    unifyTwoArguments(term[0], term[1], if (following) -1 else at, beforePending = {
                        if (program.code[at + 3] == Op.U_X_VAR && program.code[at + 6] == Op.U_X_VAR) program.code[at] = Op.GET_STR_VV
                    })
                } else {
                    program.emit(Op.GET_STRN, program.functorWord(term.functor, term.arity), register)
                    program.emit(term.arity)
                    val pending = ArrayList<Pair<Int, Term>>()
                    for (i in 0 until term.arity) subInstruction(term[i], pending)
                    for ((temp, argument) in pending) getArgument(argument, temp)
                }
            }
            else -> emitter.emitGetConst(constant(term), register)
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
        mergingFrom: Int = -1,
        beforePending: () -> Unit = {},
    ) {
        val pending = ArrayList<Pair<Int, Term>>()
        subInstruction(first, pending)
        subInstruction(second, pending)
        beforePending()
        // (a nested list merges with the one entering it, the first of what is pending)
        if (mergingFrom >= 0) emitter.merging(mergingFrom)
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
                    emitNewInSubInstruction(argument, info)
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

    // The arguments of [goal] into their registers, a register read as an argument put before it is written (a cycle broken
    // through a temporary)
    private fun putArguments(goal: Struct) {
        val n = goal.arity
        var moving = false
        for (k in 0 until n) if (registerOf(goal[k]).let { it != -1 && it != k }) moving = true
        if (!moving) {
            for (k in 0 until n) putArgument(goal[k], k)
            return
        }
        val pending = (0 until n).toMutableList()
        while (pending.isNotEmpty()) {
            val next = pending.firstOrNull { k -> pending.none { other -> other != k && registerOf(goal[other]) == k } }
            if (next != null) {
                putArgument(goal[next], next)
                pending.remove(next)
            } else {
                val first = pending.first { registerOf(goal[it]) != -1 }
                val info = vars[goal[first] as Var]!!
                val temp = nextTemp++
                program.emit(Op.PUT_X_VAL, info.register, temp)
                info.register = temp
            }
        }
    }

    // The register a goal's argument is read from, -1 if it is no variable already in one
    private fun registerOf(term: Term): Int {
        if (term !is Var) return -1
        val info = vars[term]!!
        return if (info.seen && !info.permanent) info.register else -1
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
                        val at = program.codeTop
                        program.emit(Op.PUT_Y_VAR, info.register, register, program.nameOperand(term))
                        emitter.merging(at)
                    } else {
                        // (a variable with no other occurrence has no use for a register of its own)
                        info.register = if (info.occurrences == 1) register else temporary(info)
                        program.emit(Op.PUT_X_VAR, info.register, register, program.nameOperand(term))
                    }
                } else if (info.permanent) {
                    if (!placingFirstGoal || stillInArgument[term] != register) emitter.emitPutYVal(info.register, register)
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
                if (arguments.size == 2) {
                    val at = program.codeTop
                    val following = term !is Cons && emitter.lastIs(Op.PUT_STR2)
                    if (following) emitter.rewriteLast(Op.PUT_STR2_PUT_STR2)
                    if (term is Cons) {
                        program.emit(Op.PUT_LIST2, register)
                    } else {
                        program.emit(Op.PUT_STR2, program.functorWord(term.functor, term.arity), register)
                    }
                    for ((i, argument) in arguments.withIndex()) buildSubInstruction(argument, built[i])
                    if (term !is Cons && !following) emitter.merging(at)
                    return
                }
                program.emit(Op.PUT_STR, program.functorWord(term.functor, term.arity), register)
                var voids = 0

                fun flushVoids() {
                    if (voids > 0) program.emit(Op.SET_VOID, voids)
                    voids = 0
                }
                for ((i, argument) in arguments.withIndex()) {
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
                            flushVoids()
                            program.emit(Op.SET_CONST, constant(argument))
                        }
                        is Struct -> {
                            flushVoids()
                            program.emit(Op.SET_X_VAL, built[i]!!)
                        }
                        else -> {
                            flushVoids()
                            program.emit(Op.SET_CONST, constant(argument))
                        }
                    }
                }
                flushVoids()
            }
            else -> program.emit(Op.PUT_CONST, constant(term), register)
        }
    }

    // arg/3 whose value is a permanent met here, with its index and term known: the arguments and the call only for what
    // the instruction before them leaves to the builtin
    private fun argToSlot(
        goal: Struct,
        functor: Int,
    ): Boolean {
        val index = goal[0] as? Var ?: return false
        val term = goal[1] as? Var ?: return false
        val value = goal[2] as? Var ?: return false
        val info = vars[value]!!
        if (!vars[index]!!.seen || !vars[term]!!.seen || !info.permanent || info.seen || info.occurrences == 1) return false
        program.emit(Op.ARG_TO_SLOT, operand(index), operand(term), info.register)
        val skipAt = program.emit(0)
        emitter.breakMerging()
        val rest = program.codeTop
        placingFirstGoal = goal === firstGoal
        putArgument(index, 0)
        putArgument(term, 1)
        if (placingFirstGoal) stillInArgument.clear()
        placingFirstGoal = false
        putArgument(value, 2)
        program.emit(Op.CALL, functor, goal.arity)
        program.code[skipAt] = program.codeTop - rest
        emitter.breakMerging()
        return true
    }

    // A variable met first as an argument of a structure
    private fun emitNewInSubInstruction(
        variable: Var,
        info: VarInfo,
    ) {
        info.seen = true
        if (info.permanent) {
            program.emit(Op.U_Y_VAR, info.register, program.nameOperand(variable))
        } else {
            info.register = temporary(info)
            program.emit(Op.U_X_VAR, info.register, program.nameOperand(variable))
        }
    }

    // An argument of PUT_STR2 or PUT_LIST2 (compound: already built in [temp])
    private fun buildSubInstruction(
        argument: Term,
        temp: Int?,
    ) {
        when {
            temp != null -> program.emit(Op.U_X_VAL, temp, 0)
            argument is Var -> {
                val info = vars[argument]!!
                when {
                    info.occurrences == 1 -> program.emit(Op.U_VOID, 0, 0)
                    info.seen -> program.emit(if (info.permanent) Op.U_Y_VAL else Op.U_X_VAL, info.register, 0)
                    else -> emitNewInSubInstruction(argument, info)
                }
            }
            else -> program.emit(Op.U_CONST, constant(argument), 0)
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
        program.emit(Op.RETURN)
        emitter.placeLabel(elseLabel)
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
        emitter.placeLabel(elseLabel)
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
        emitter.placeLabel(elseLabel)
        program.emit(Op.TRUST_ME)
        program.emit(Op.RETURN)
        return start
    }

    /**
     * The code an inlined goal returns to: the scope of the caller restored from the frame the inlining allocated
     * (see Machine.inline), the frame deallocated
     */
    fun restoreScopeBlock(): Int {
        val start = program.codeTop
        program.emit(Op.RESTORE_SCOPE_RETURN)
        return start
    }

    /** The address of a `STOP` instruction: the continuation of a run that ends with a solution */
    fun stopBlock(): Int {
        val start = program.codeTop
        program.emit(Op.STOP)
        return start
    }
}
