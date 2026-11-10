package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.constraints.helpers

/**
 * A linear constraint over variables numbered from 0: the sum of [coefficients] (by variable) times the variables is
 * at most [constant] ([Kind.AT_MOST]), below it ([Kind.BELOW]) or equal to it ([Kind.EQUAL])
 */
internal class LinearRow(
    val coefficients: Map<Int, Rational>,
    val constant: Rational,
    val kind: Kind,
) {
    enum class Kind { AT_MOST, BELOW, EQUAL }

    override fun toString(): String =
        coefficients.entries.joinToString(" + ") { (variable, coefficient) -> "$coefficient*x$variable" }.ifEmpty { "0" } +
            when (kind) {
                Kind.AT_MOST -> " =< "
                Kind.BELOW -> " < "
                Kind.EQUAL -> " = "
            } + constant
}

/**
 * Whether linear constraints can hold together over the rationals (numbers are dense): the simplex method, on exact
 * rationals, Bland's rule against cycling. A strict constraint `a < b` is `a + e =< b` with `e` maximised (`e =< 1`):
 * the constraints hold together when they do with `e` above 0. Each variable may be negative: it is the difference of
 * two non-negative ones. The systems are those of the arguments, a few constraints: dense tableaus.
 */
internal object LinearSystem {
    fun satisfiable(rows: List<LinearRow>): Boolean {
        if (rows.isEmpty()) return true
        val variables = (rows.flatMap { it.coefficients.keys }.maxOrNull() ?: -1) + 1
        val strict = rows.any { it.kind == LinearRow.Kind.BELOW }
        // the columns: x+ (variables), x- (variables), then e if strict
        val epsilon = 2 * variables
        val columns = epsilon + if (strict) 1 else 0
        val a = ArrayList<Array<Rational>>()
        val b = ArrayList<Rational>()

        fun add(
            row: LinearRow,
            sign: Rational,
        ) {
            val coefficients = Array(columns) { Rational.ZERO }
            for ((variable, coefficient) in row.coefficients) {
                coefficients[variable] = coefficient * sign
                coefficients[variables + variable] = -(coefficient * sign)
            }
            if (row.kind == LinearRow.Kind.BELOW) coefficients[epsilon] = Rational.ONE
            a += coefficients
            b += row.constant * sign
        }
        for (row in rows) {
            add(row, Rational.ONE)
            if (row.kind == LinearRow.Kind.EQUAL) add(row, -Rational.ONE)
        }
        if (strict) {
            a += Array(columns) { if (it == epsilon) Rational.ONE else Rational.ZERO }
            b += Rational.ONE
        }
        val objective = Array(columns) { if (strict && it == epsilon) Rational.ONE else Rational.ZERO }
        val best = Simplex(a, b, objective).maximum() ?: return false
        return !strict || best.signum > 0
    }
}

/**
 * Maximise objective·y with a·y =< b, y >= 0, in a full tableau: the slacks of the rows start as the basis, and an
 * artificial variable finds a first feasible basis when some b is negative (the two phases)
 */
private class Simplex(
    a: List<Array<Rational>>,
    b: List<Rational>,
    private val objective: Array<Rational>,
) {
    private val rows = a.size
    private val original = objective.size

    // the columns: the original variables, the slacks, the artificial one; then the right-hand side
    private val artificial = original + rows
    private val width = artificial + 1
    private val tableau = Array(rows) { i -> Array(width + 1) { j -> cell(a[i], b[i], i, j) } }
    private val basis = IntArray(rows) { original + it }

    // z = value + sum of costs[j] * y[j], over the variables out of the basis
    private var costs = Array(width) { Rational.ZERO }
    private var value = Rational.ZERO

    private fun cell(
        row: Array<Rational>,
        constant: Rational,
        i: Int,
        j: Int,
    ): Rational =
        when {
            j < original -> row[j]
            j == original + i -> Rational.ONE
            j == artificial -> -Rational.ONE
            j == width -> constant
            else -> Rational.ZERO
        }

    /** The maximum, null if no y satisfies the constraints (the objective is bounded for the systems built above) */
    fun maximum(): Rational? {
        val lowest = (0 until rows).minByOrNull { tableau[it][width] }
        if (lowest != null && tableau[lowest][width].signum < 0) {
            // phase 1: maximise -artificial, entering at the most negative row, which makes every row feasible
            costs = Array(width) { if (it == artificial) -Rational.ONE else Rational.ZERO }
            value = Rational.ZERO
            pivot(lowest, artificial)
            if (!optimise()) return null
            if (value.signum < 0) return null
            val at = basis.indexOf(artificial)
            if (at >= 0) {
                // (at 0: out of the basis by any other column of its row, or the row says nothing)
                val column = (0 until artificial).firstOrNull { !tableau[at][it].isZero }
                if (column != null) pivot(at, column)
            }
        }
        // phase 2: the objective over the original variables, the artificial one at 0 and out
        for (i in 0 until rows) tableau[i][artificial] = Rational.ZERO
        costs = Array(width) { if (it < original) objective[it] else Rational.ZERO }
        value = Rational.ZERO
        for (i in 0 until rows) {
            val cost = costs[basis[i]]
            if (cost.isZero) continue
            for (j in 0 until width) costs[j] = costs[j] - cost * tableau[i][j]
            value = value + cost * tableau[i][width]
        }
        if (!optimise()) return Rational.of(Long.MAX_VALUE)
        return value
    }

    // Pivots until no cost is positive: false if the objective is unbounded
    private fun optimise(): Boolean {
        while (true) {
            val entering = (0 until width).firstOrNull { costs[it].signum > 0 } ?: return true
            var leaving = -1
            var ratio = Rational.ZERO
            for (i in 0 until rows) {
                val coefficient = tableau[i][entering]
                if (coefficient.signum <= 0) continue
                val candidate = tableau[i][width] / coefficient
                if (leaving < 0 || candidate < ratio || (candidate == ratio && basis[i] < basis[leaving])) {
                    leaving = i
                    ratio = candidate
                }
            }
            if (leaving < 0) return false
            pivot(leaving, entering)
        }
    }

    private fun pivot(
        r: Int,
        e: Int,
    ) {
        val row = tableau[r]
        val factor = row[e]
        for (j in 0..width) row[j] = row[j] / factor
        for (i in 0 until rows) {
            if (i == r) continue
            val coefficient = tableau[i][e]
            if (coefficient.isZero) continue
            for (j in 0..width) tableau[i][j] = tableau[i][j] - coefficient * row[j]
        }
        val cost = costs[e]
        if (!cost.isZero) {
            for (j in 0 until width) costs[j] = costs[j] - cost * row[j]
            value = value + cost * row[width]
        }
        basis[r] = e
    }
}
