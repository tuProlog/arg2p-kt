package it.unibo.tuprolog.argumentation.core.primitives.graph.extension

import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Numeric
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve
import org.gciatto.kt.math.BigDecimal

/**
 * The regions of the constraints extension: sets of numbers a statement holds for, written in place of a number.
 * `from(T)` is the region after T, `until(T)` the one up to T included, `between(A, B)` the one after A up to B
 * included, and a number the region of that number alone.
 */
internal data class Region(
    val low: BigDecimal?,
    val lowClosed: Boolean,
    val high: BigDecimal?,
    val highClosed: Boolean,
) {
    /** Whether [other] is within this region */
    fun contains(other: Region): Boolean = lowAtMost(other) && highAtLeast(other)

    /** Whether this region and [other] have a number in common */
    fun overlaps(other: Region): Boolean {
        val lowest = maxLow(other)
        val highest = minHigh(other)
        val (low, lowClosed) = lowest
        val (high, highClosed) = highest
        if (low == null || high == null) return true
        val order = low.compareTo(high)
        return order < 0 || (order == 0 && lowClosed && highClosed)
    }

    private fun lowAtMost(other: Region): Boolean {
        if (low == null) return true
        if (other.low == null) return false
        val order = low.compareTo(other.low)
        return order < 0 || (order == 0 && (lowClosed || !other.lowClosed))
    }

    private fun highAtLeast(other: Region): Boolean {
        if (high == null) return true
        if (other.high == null) return false
        val order = high.compareTo(other.high)
        return order > 0 || (order == 0 && (highClosed || !other.highClosed))
    }

    private fun maxLow(other: Region): Pair<BigDecimal?, Boolean> =
        when {
            low == null -> other.low to other.lowClosed
            other.low == null -> low to lowClosed
            else ->
                low.compareTo(other.low).let {
                    when {
                        it > 0 -> low to lowClosed
                        it < 0 -> other.low to other.lowClosed
                        else -> low to (lowClosed && other.lowClosed)
                    }
                }
        }

    private fun minHigh(other: Region): Pair<BigDecimal?, Boolean> =
        when {
            high == null -> other.high to other.highClosed
            other.high == null -> high to highClosed
            else ->
                high.compareTo(other.high).let {
                    when {
                        it < 0 -> high to highClosed
                        it > 0 -> other.high to other.highClosed
                        else -> high to (highClosed && other.highClosed)
                    }
                }
        }

    companion object {
        /** The region a term writes (a region term, or a number), null for any other term */
        fun of(term: Term): Region? =
            when {
                term is Numeric -> term.decimalValue.let { Region(it, true, it, true) }
                term is Struct && term.arity == 1 && term.functor == "from" ->
                    (term[0] as? Numeric)?.let { Region(it.decimalValue, false, null, false) }
                term is Struct && term.arity == 1 && term.functor == "until" ->
                    (term[0] as? Numeric)?.let { Region(null, false, it.decimalValue, true) }
                term is Struct && term.arity == 2 && term.functor == "between" -> {
                    val low = term[0] as? Numeric
                    val high = term[1] as? Numeric
                    if (low != null && high != null) Region(low.decimalValue, false, high.decimalValue, true) else null
                }
                else -> null
            }

        /** Whether [term] is a region term (not a number) */
        fun isRegionTerm(term: Term): Boolean = term !is Numeric && of(term) != null
    }
}

private fun Solve.Request<ExecutionContext>.unifyWith(
    term: Term,
    value: Term,
): Sequence<Solve.Response> = Arg2pUnificator.mgu(term, value).let { sequenceOf(if (it.isSuccess) replyWith(it) else replyFail()) }

/** regionContains(+Outer, +Inner): Inner, a region term or a number, is within Outer, a region term or a number */
internal object RegionContains : PrimitiveWithSignature {
    override val signature = Signature("regionContains", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val outer = Region.of(request.arguments[0])
        val inner = Region.of(request.arguments[1])
        return sequenceOf(request.replyWith(outer != null && inner != null && outer.contains(inner)))
    }
}

/** regionOverlaps(+A, +B): A and B, region terms or numbers, have a number in common */
internal object RegionOverlaps : PrimitiveWithSignature {
    override val signature = Signature("regionOverlaps", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val a = Region.of(request.arguments[0])
        val b = Region.of(request.arguments[1])
        return sequenceOf(request.replyWith(a != null && b != null && a.overlaps(b)))
    }
}

/**
 * regionsAbstract(+Term, -Abstract, -Pairs): Term with each of its region terms replaced by a variable of its own,
 * Pairs the `Var-Region` pairs; valuesAbstract/3 replaces the numbers as well
 */
internal class Abstraction(
    name: String,
    private val numbers: Boolean,
) : PrimitiveWithSignature {
    override val signature = Signature(name, 3)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val pairs = mutableListOf<Term>()
        val abstract = abstracted(request.arguments[0], pairs)
        val unifier =
            Arg2pUnificator.mgu(
                Struct.of("-", request.arguments[1], request.arguments[2]),
                Struct.of("-", abstract, List.of(pairs)),
            )
        return sequenceOf(if (unifier.isSuccess) request.replyWith(unifier) else request.replyFail())
    }

    private fun abstracted(
        term: Term,
        pairs: MutableList<Term>,
    ): Term =
        when {
            Region.isRegionTerm(term) || (numbers && term is Numeric) ->
                Var.anonymous().also { pairs += Struct.of("-", it, term) }
            term is Struct && term.arity > 0 -> Struct.of(term.functor, term.args.map { abstracted(it, pairs) })
            else -> term
        }

    companion object {
        val REGIONS = Abstraction("regionsAbstract", false)
        val VALUES = Abstraction("valuesAbstract", true)
    }
}

/** regionReplace(+Term, +Map, -Replaced): Term with each subterm equal to a From of Map (From-To pairs) replaced by To */
internal object RegionReplace : PrimitiveWithSignature {
    override val signature = Signature("regionReplace", 3)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val map =
            request.arguments[1].asList()!!.toList().associate {
                val pair = it.castToStruct()
                pair[0] to pair[1]
            }
        return request.unifyWith(request.arguments[2], replaced(request.arguments[0], map))
    }

    private fun replaced(
        term: Term,
        map: Map<Term, Term>,
    ): Term = map[term] ?: if (term is Struct && term.arity > 0) Struct.of(term.functor, term.args.map { replaced(it, map) }) else term
}
