package it.unibo.tuprolog.argumentation.engine.vm.compile

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var

internal class VarInfo {
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

/**
 * The analysis of a clause before it is compiled: the chunks each variable occurs in (the head and the first goal, then each
 * goal called), which makes it temporary or permanent; the goals called, the control constructs and the cuts, which say
 * whether the clause needs a frame; the goal of the first chunk and the goals inline before it, which the registers of the
 * head may reach. One clause at a time: [vars] and the rest are the clause last analysed.
 */
internal class ClauseAnalysis(
    private val inlining: Inlining,
) {
    /** Whether the clause last analysed needs a frame: permanent variables, more than one goal called, a call not the last
     * of the clause, a control construct, or a cut after a goal */
    var needsFrame = false
        private set

    /** The clause of [head] and [body] analysed: its permanent variables numbered (their slots), how many */
    fun analyse(
        head: Struct,
        body: ClauseBody,
    ): Int {
        vars.clear()
        firstGoal = null
        inlineGoals.clear()
        inlineOpen = false
        chunk = 0
        leafGoals = 0
        callFollowed = false
        hasControl = false
        cutAfterGoal = false
        levels = 0
        maxArity = head.arity
        for (i in 0 until head.arity) occurrences(head[i], 0, region = false)
        analyze(body, first = true, region = false)
        var permanents = 0
        for (info in vars.values) {
            info.permanent = info.inMoreChunks
            if (info.permanent) info.register = permanents++
        }
        needsFrame = permanents > 0 || leafGoals > 1 || hasControl || cutAfterGoal || callFollowed
        return permanents
    }

    val vars = LinkedHashMap<Var, VarInfo>()

    var chunk = 0
        private set

    var leafGoals = 0
        private set

    // Whether a goal compiled inline follows a call: the call is then not the last of the clause, which needs a frame to keep
    // its continuation across it
    var callFollowed = false
        private set

    var hasControl = false
        private set

    var cutAfterGoal = false
        private set

    // The goals compiled inline before the first goal of the clause
    val inlineGoals = ArrayList<Struct>()

    var levels = 0
        private set

    var maxArity = 0
        private set

    // The goal of the first chunk (the one the head's temporaries may reach), if any
    var firstGoal: Struct? = null

    // Whether the goal analysed last was arithmetic compiled inline: the next call joins its chunk (no call between)
    var inlineOpen = false
        private set

    fun analyze(
        body: ClauseBody,
        first: Boolean,
        region: Boolean,
    ) {
        when (body) {
            is ClauseBody.Goal -> {
                val goal = body.term
                val inline = inlining.inlineArithmetic(goal) || inlining.inlineBuiltin(goal)
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
                    val called = inlining.moduleGoalOf(goal) ?: goal
                    if (called.arity > maxArity) maxArity = called.arity
                    if (c == 0 && firstGoal == null) firstGoal = called
                } else {
                    if (leafGoals > 0) callFollowed = true
                    if (c == 0 && firstGoal == null) inlineGoals.add(goal)
                }
                inlineOpen = inline
                for (i in 0 until goal.arity) occurrences(goal[i], c, region)
            }
            is ClauseBody.Conj -> {
                // (a cut neither moves nor reads a register: the goal after a leading one is still in the chunk of the head)
                var leading = first
                for (item in body.items) {
                    analyze(item, leading, region)
                    if (item !is ClauseBody.Cut) leading = false
                }
            }
            is ClauseBody.Disj -> analyzeControl(body.term, first, region, level = false, body.left, body.right)
            is ClauseBody.IfThenElse -> analyzeControl(body.term, first, region, level = true, body.condition, body.then, body.otherwise)
            is ClauseBody.Naf -> analyzeControl(body.term, first, region, level = true, body.goal)
            ClauseBody.Cut -> if (leafGoals > 0) cutAfterGoal = true
            ClauseBody.True -> Unit
        }
    }

    // A control construct compiled inline, its [parts] each a region of its own (and a level, a frame slot, if [level]: a cut
    // inside it is local to it); a goal like any other when the clauses define it
    fun analyzeControl(
        term: Struct,
        first: Boolean,
        region: Boolean,
        level: Boolean,
        vararg parts: ClauseBody,
    ) {
        if (inlining.definedByTheory(term)) return analyze(ClauseBody.Goal(term), first, region)
        hasControl = true
        if (level) levels++
        for (part in parts) {
            inlineOpen = false
            analyze(part, first = false, region = true)
        }
        inlineOpen = false
    }

    fun occurrences(
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
}
