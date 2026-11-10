package it.unibo.tuprolog.argumentation.engine.vm.compile

import it.unibo.tuprolog.argumentation.engine.term.collectVariables
import it.unibo.tuprolog.argumentation.engine.vm.program.ClauseCode
import it.unibo.tuprolog.argumentation.engine.vm.program.Numbers
import it.unibo.tuprolog.argumentation.engine.vm.program.Op
import it.unibo.tuprolog.argumentation.engine.vm.program.Program
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

    private val emitter = CodeEmitter(program)

    // What compiles inline, and the analysis of the clause at hand
    private val inlining = Inlining(program)
    private val analysis = ClauseAnalysis(inlining)

    /** The clauses compiled from now on, until the next call, come with [clauses] (see [Inlining.definedAlongside]) */
    fun definedAlongside(clauses: Iterable<Clause>) = inlining.definedAlongside(clauses)

    private val heads = ClauseHeads(program)

    private fun constant(term: Term): Int = program.constantWord(term)

    // The permanents that are one argument of the head and of the first goal alike, by that argument: the register still
    // holds them when the goal is called (temporaries live above, the only one placed there is the goal's own)
    private val stillInArgument = HashMap<Var, Int>()
    private var placingFirstGoal = false

    private var nextTemp = 0
    private var nextLevel = 0

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
        val keys = IntArray(head.arity) { heads.keyOf(head[it]) }
        return ClauseCode(
            keys,
            start,
            clause,
            dynamic,
            heads.innerKeysOf(head),
            heads.guardedFrom(start, head.arity),
            heads.commitMaskOf(head, clause.body),
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

    private fun compile(
        head: Struct,
        bodyTerm: Term,
    ): Int {
        cutLevels.clear()
        stillInArgument.clear()
        placingFirstGoal = false
        val body = ClauseBody.parse(bodyTerm)
        val permanents = analysis.analyse(head, body)
        emitter.needsEnv = analysis.needsFrame
        preferRegisters(head)
        nextTemp = analysis.maxArity
        nextLevel = permanents
        val start = program.codeTop
        emitter.openClause(permanents + analysis.levels)
        for ((variable, info) in analysis.vars) {
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
        val goal = analysis.firstGoal
        if (goal == null) {
            for (i in 0 until head.arity) if (firstIn[head[i]] == i && head[i] in bare) keepInArgument(head[i], i)
            return
        }
        for (k in 0 until goal.arity) {
            val variable = goal[k] as? Var ?: continue
            val info = analysis.vars[variable]!!
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
            val placed = taking != null && analysis.vars[taking]!!.preferred == i
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
        val info = analysis.vars[term]!!
        if (!info.permanent && info.occurrences > 1 && info.preferred == -1) info.preferred = i
    }

    // Whether the variable [taking] gets its register, which [variable] lives in, no sooner than the last read of [variable]:
    // created by an `is` of the inline goals, reading it there at the latest (as it computes before it stores)
    private fun overwrittenAfterLastRead(
        variable: Var,
        taking: Var,
    ): Boolean {
        var created = false
        for (goal in analysis.inlineGoals) {
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

    // ---- arithmetic compiled inline

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
            val info = analysis.vars[variable]!!
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
            if (inlining.unifiesInline(
                    goal[0],
                    goal[1],
                )
            ) {
                emitUnification(goal[0] as Var, goal[1])
            } else {
                emitUnification(goal[1] as Var, goal[0])
            }
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
        val info = analysis.vars[term]!!
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

    private fun operand(variable: Var): Int = analysis.vars[variable]!!.let { if (it.permanent) -1 - it.register else it.register }

    // variable = other: other unified against the variable, or built as its first value
    private fun emitUnification(
        variable: Var,
        other: Term,
    ) {
        val info = analysis.vars[variable]!!
        when {
            other == variable -> created(variable)
            info.seen -> unifyWith(info, other)
            other is Var && analysis.vars[other]!!.seen -> unifyWith(analysis.vars[other]!!, variable)
            other is Var -> {
                created(other)
                unifyWith(analysis.vars[other]!!, variable)
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
            emitTarget(target)
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
            expression.functor == "+" && left is Var && right is Integer && inlining.smallInteger(right) -> left to right.value.toInt()
            expression.functor == "+" && right is Var && left is Integer && inlining.smallInteger(left) -> right to left.value.toInt()
            expression.functor == "-" && left is Var && right is Integer && inlining.smallInteger(right) -> left to -right.value.toInt()
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
            left is Var &&
                right is Integer &&
                inlining.smallInteger(
                    right,
                ) -> program.emit(Op.COMPARE_VC, kind, operand(left), right.value.toInt())
            right is Var &&
                left is Integer &&
                inlining.smallInteger(
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
                val info = analysis.vars[term]!!
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
                val info = analysis.vars[term]!!
                program.emit(if (info.permanent) Op.E_Y else Op.E_X, info.register)
            }
            is Integer -> program.emit(Op.E_CONST, constant(term))
            is Struct -> {
                for (i in 0 until term.arity) emitElements(term[i])
                program.emit(inlining.operation(term), 0)
            }
            else -> throw IllegalStateException("not an expression: $term")
        }
    }

    // ---- emission

    /** Emits [body]; true if it ended by executing a goal in place (the clause then needs no return) */
    private fun emit(
        body: ClauseBody,
        last: Boolean,
    ): Boolean {
        when (body) {
            is ClauseBody.Goal -> {
                val goal = body.term
                if (inlining.inlineArithmetic(goal)) {
                    emitArithmetic(goal)
                    return false
                }
                if (inlining.inlineBuiltin(goal)) {
                    emitBuiltin(goal)
                    return false
                }
                val moduleGoal = inlining.moduleGoalOf(goal)
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
                val isArg = goal.functor == "arg" && goal.arity == 3 && !inlining.userDefines(goal)
                if (isArg && !last && argToSlot(goal, functor)) return false
                placingFirstGoal = goal === analysis.firstGoal
                putArguments(goal)
                if (placingFirstGoal) stillInArgument.clear()
                placingFirstGoal = false
                if (isArg) program.emit(Op.ARG_CALL)
                emitter.emitCall(last, functor, goal.arity)
                return last
            }
            is ClauseBody.Conj -> {
                val items = body.items
                for ((i, item) in items.withIndex()) {
                    if (emit(item, last && i == items.size - 1)) return true
                }
                return false
            }
            is ClauseBody.Disj -> {
                if (inlining.definedByTheory(body.term)) return emit(ClauseBody.Goal(body.term), last)
                program.emit(Op.TRY_ELSE)
                val elseLabel = program.emit(0)
                val endLabel = emitBranchEnd(emit(body.left, last), last)
                emitter.placeLabel(elseLabel)
                program.emit(Op.TRUST_ME)
                return emitLastBranch(emit(body.right, last), last, endLabel)
            }
            is ClauseBody.IfThenElse -> {
                if (inlining.definedByTheory(body.term)) return emit(ClauseBody.Goal(body.term), last)
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
            is ClauseBody.Naf -> {
                if (inlining.definedByTheory(body.term)) return emit(ClauseBody.Goal(body.term), last)
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
            ClauseBody.Cut -> {
                // (a cut inside a negation or a condition is local to it: the choice point of the construct stays)
                when {
                    cutLevels.isNotEmpty() -> program.emit(Op.CUT_LOCAL, cutLevels[cutLevels.size - 1])
                    else -> emitter.emitCut()
                }
                return false
            }
            ClauseBody.True -> return false
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
                val info = analysis.vars[term]!!
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
                    val h = analysis.vars[head]!!
                    val t = analysis.vars[tail]!!
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
        val info = analysis.vars[variable]!!
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
                val info = analysis.vars[argument]!!
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
                val info = analysis.vars[goal[first] as Var]!!
                val temp = nextTemp++
                program.emit(Op.PUT_X_VAL, info.register, temp)
                info.register = temp
            }
        }
    }

    // The register a goal's argument is read from, -1 if it is no variable already in one
    private fun registerOf(term: Term): Int {
        if (term !is Var) return -1
        val info = analysis.vars[term]!!
        return if (info.seen && !info.permanent) info.register else -1
    }

    private fun putArgument(
        term: Term,
        register: Int,
    ) {
        when (term) {
            is Var -> {
                val info = analysis.vars[term]!!
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
                            val info = analysis.vars[argument]!!
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
        val info = analysis.vars[value]!!
        if (!analysis.vars[index]!!.seen ||
            !analysis.vars[term]!!.seen ||
            !info.permanent ||
            info.seen ||
            info.occurrences == 1
        ) {
            return false
        }
        program.emit(Op.ARG_TO_SLOT, operand(index), operand(term), info.register)
        val skipAt = program.emit(0)
        emitter.breakMerging()
        val rest = program.codeTop
        placingFirstGoal = goal === analysis.firstGoal
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
                val info = analysis.vars[argument]!!
                when {
                    info.occurrences == 1 -> program.emit(Op.U_VOID, 0, 0)
                    info.seen -> program.emit(if (info.permanent) Op.U_Y_VAL else Op.U_X_VAL, info.register, 0)
                    else -> emitNewInSubInstruction(argument, info)
                }
            }
            else -> program.emit(Op.U_CONST, constant(argument), 0)
        }
    }
}
