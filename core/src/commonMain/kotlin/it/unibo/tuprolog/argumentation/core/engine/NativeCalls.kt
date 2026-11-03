package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.InstantiationError
import it.unibo.tuprolog.solve.exception.error.TypeError

// The helpers of a NativePrimitive: its arguments read and checked, its replies and its answers

/** The items of [list] if it is a proper list, null otherwise */
fun NativeCall.items(list: Int): IntArray? {
    var items = IntArray(8)
    var n = 0
    var w = list
    while (kind(w) == NativeCall.LIST) {
        if (n == items.size) items = items.copyOf(n * 2)
        items[n++] = argOf(w, 0)
        w = argOf(w, 1)
    }
    if (kind(w) != NativeCall.ATOM || atomName(w) != "[]") return null
    return items.copyOf(n)
}

/** Whether [w] is a proper list */
fun NativeCall.isList(w: Int): Boolean = items(w) != null

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

/** The argument [i], a list, as terms (a type error otherwise) */
fun NativeCall.termsArg(
    i: Int,
    signature: Signature,
): List<Term> = listArg(i, signature).map { term(it) }

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
 * alternative left after the last
 */
fun NativeCall.answers(
    i: Int,
    values: Iterator<Term>,
): Int {
    while (values.hasNext()) {
        val value = values.next()
        if (values.hasNext()) {
            alternatives { it.answers(i, values) }
            return reply(i, value)
        }
        return reply(i, value)
    }
    return NativeCall.FAIL
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
