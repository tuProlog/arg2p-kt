package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.argumentation.core.runtime.renamedCopy
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.InstantiationError
import it.unibo.tuprolog.solve.exception.error.TypeError

// The helpers of a NativePrimitive: its arguments read and checked, its replies and its answers

/** The items of [list] if it is a proper list, null otherwise */
fun NativeCall.items(list: Int): IntArray? {
    val n = listLength(list)
    if (n < 0) return null
    val items = IntArray(n)
    var w = list
    for (i in 0 until n) {
        items[i] = argOf(w, 0)
        w = argOf(w, 1)
    }
    return items
}

// How long the proper list [list] is, -1 if it is none
private fun NativeCall.listLength(list: Int): Int {
    var n = 0
    var w = list
    while (kind(w) == NativeCall.LIST) {
        n++
        w = argOf(w, 1)
    }
    return if (kind(w) == NativeCall.ATOM && atomName(w) == "[]") n else -1
}

/** Whether [w] is a proper list */
fun NativeCall.isList(w: Int): Boolean = listLength(w) >= 0

/** The items of the argument [i], a list (a type error otherwise, from the primitive of [signature]) */
fun NativeCall.listArg(
    i: Int,
    signature: Signature,
): IntArray = items(arg(i)) ?: throw TypeError.forArgument(context, signature, TypeError.Expected.LIST, term(arg(i)), i)

/** The argument [i], an integer (an instantiation or a type error otherwise) */
fun NativeCall.intArg(
    i: Int,
    signature: Signature,
): Long {
    val w = arg(i)
    return when (kind(w)) {
        NativeCall.INT -> intValue(w)
        NativeCall.VAR -> throw InstantiationError.forArgument(context, signature, term(w).castToVar(), i)
        else -> throw TypeError.forArgument(context, signature, TypeError.Expected.INTEGER, term(w), i)
    }
}

/** The argument [i], an atom (an instantiation or a type error otherwise) */
fun NativeCall.atomArg(
    i: Int,
    signature: Signature,
): String {
    val w = arg(i)
    return when (kind(w)) {
        NativeCall.ATOM -> atomName(w)
        NativeCall.VAR -> throw InstantiationError.forArgument(context, signature, term(w).castToVar(), i)
        else -> throw TypeError.forArgument(context, signature, TypeError.Expected.ATOM, term(w), i)
    }
}

/** The argument [i] as a term */
fun NativeCall.termArg(i: Int): Term = term(arg(i))

fun NativeCall.words(terms: List<Term>): IntArray = IntArray(terms.size) { word(terms[it]) }

/** The argument [i] unified with [w]: [NativeCall.CONTINUE] or [NativeCall.FAIL] */
fun NativeCall.replyWord(
    i: Int,
    w: Int,
): Int = if (unify(arg(i), w)) NativeCall.CONTINUE else NativeCall.FAIL

fun NativeCall.reply(
    i: Int,
    term: Term,
): Int = replyWord(i, word(term))

/** The argument [i] unified with the list of [items] */
fun NativeCall.replyItems(
    i: Int,
    items: IntArray,
): Int = replyWord(i, list(items))

fun NativeCall.replyTerms(
    i: Int,
    terms: List<Term>,
): Int = replyItems(i, words(terms))

/** Done, or failed, by [success] */
fun NativeCall.reply(success: Boolean): Int = if (success) NativeCall.CONTINUE else NativeCall.FAIL

/**
 * The argument [i] unified with each of [values] in turn, on backtracking: an answer per value that unifies, no
 * alternative left after the last of those (the values are matched against the argument as terms first, so that
 * a call with one answer leaves no choice point, as the facade leaves none)
 */
fun NativeCall.answers(
    i: Int,
    values: Iterator<Term>,
): Int {
    val target = term(arg(i))
    val unificator = context.unificator
    return attempts(
        values
            .asSequence()
            .filter { unificator.mgu(target, it).isSuccess }
            .map { value -> { call: NativeCall -> call.reply(i, value) == NativeCall.CONTINUE } }
            .iterator(),
    )
}

/**
 * An answer per candidate of [candidates] (a tuple of terms, its variables renamed apart as those of a clause),
 * the arguments of the call from [from] on unified with it, on backtracking
 */
fun NativeCall.answersMatching(
    from: Int,
    candidates: Iterator<List<Term>>,
): Int {
    val target = Struct.of("t", (from until arity).map { term(arg(it)) })
    val unificator = context.unificator
    return attempts(
        candidates
            .asSequence()
            .map { values -> Struct.of("t", values).renamedCopy() }
            .filter { unificator.mgu(target, it).isSuccess }
            .map { renamed ->
                { call: NativeCall ->
                    var ok = true
                    for (i in 0 until renamed.arity) {
                        if (!call.unify(call.arg(from + i), call.word(renamed[i]))) {
                            ok = false
                            break
                        }
                    }
                    ok
                }
            }.iterator(),
    )
}

/** An answer per substitution of [substitutions] (over the arguments as terms), the arguments unified under it */
fun NativeCall.answersOf(substitutions: Iterator<Substitution>): Int {
    val arguments = List(arity) { term(arg(it)) }
    return attempts(
        substitutions
            .asSequence()
            .map { substitution ->
                { call: NativeCall ->
                    var ok = true
                    for (i in arguments.indices) {
                        if (!call.unify(call.arg(i), call.word(arguments[i].apply(substitution)))) {
                            ok = false
                            break
                        }
                    }
                    ok
                }
            }.iterator(),
    )
}

/** An answer per element of [attempts] that succeeds (each unifying what it needs), on backtracking */
fun NativeCall.attempts(attempts: Iterator<(NativeCall) -> Boolean>): Int {
    while (attempts.hasNext()) {
        val attempt = attempts.next()
        if (attempts.hasNext()) {
            alternatives { it.attempts(attempts) }
            return reply(attempt(this))
        }
        return reply(attempt(this))
    }
    return NativeCall.FAIL
}
