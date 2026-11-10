package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.constraints.helpers

import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Real
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var

/**
 * The constraints the module decides: comparisons (`<`, `=<`, `>`, `>=`, `=:=`, `=`) between linear expressions of
 * numbers and variables (`+`, `-`, a product or a quotient by a number). Anything else is a check, as prolog/1.
 */
internal object LinearTerms {
    private val comparisons = setOf("<", "=<", ">", ">=", "=:=", "=")

    /** Whether [constraint] is one the module decides */
    fun isLinear(constraint: Term): Boolean = comparison(constraint) != null

    /** The rows of [constraints], each a linear comparison, their variables numbered in the order met */
    fun rows(constraints: List<Term>): List<LinearRow> {
        val numbers = HashMap<Var, Int>()
        return constraints.map { constraint ->
            val (operator, left, right) = comparison(constraint) ?: error("not a linear constraint: $constraint")
            // left - right OP 0, that is sum OP -constant
            val difference = left.minus(right)
            val coefficients =
                difference.coefficients
                    .filterValues { !it.isZero }
                    .mapKeys { (variable, _) -> numbers.getOrPut(variable) { numbers.size } }
            val constant = -difference.constant
            when (operator) {
                "<" -> LinearRow(coefficients, constant, LinearRow.Kind.BELOW)
                "=<" -> LinearRow(coefficients, constant, LinearRow.Kind.AT_MOST)
                ">" -> LinearRow(coefficients.mapValues { -it.value }, -constant, LinearRow.Kind.BELOW)
                ">=" -> LinearRow(coefficients.mapValues { -it.value }, -constant, LinearRow.Kind.AT_MOST)
                else -> LinearRow(coefficients, constant, LinearRow.Kind.EQUAL)
            }
        }
    }

    private fun comparison(constraint: Term): Triple<String, Linear, Linear>? {
        val struct = constraint as? Struct ?: return null
        if (struct.arity != 2 || struct.functor !in comparisons) return null
        val left = linear(struct[0]) ?: return null
        val right = linear(struct[1]) ?: return null
        return Triple(struct.functor, left, right)
    }

    // A linear expression: the coefficients of its variables and its constant
    private class Linear(
        val coefficients: Map<Var, Rational>,
        val constant: Rational,
    ) {
        val isConstant: Boolean get() = coefficients.values.all { it.isZero }

        fun plus(other: Linear): Linear {
            val sum = HashMap(coefficients)
            for ((variable, coefficient) in other.coefficients) sum[variable] = (sum[variable] ?: Rational.ZERO) + coefficient
            return Linear(sum, constant + other.constant)
        }

        fun minus(other: Linear): Linear = plus(other.scaled(-Rational.ONE))

        fun scaled(factor: Rational): Linear = Linear(coefficients.mapValues { it.value * factor }, constant * factor)
    }

    private fun linear(term: Term): Linear? =
        when (term) {
            is Var -> Linear(mapOf(term to Rational.ONE), Rational.ZERO)
            is Integer -> Linear(emptyMap(), Rational.of(term.value))
            is Real -> Linear(emptyMap(), Rational.ofDecimal(term.value.toString()))
            is Struct -> operation(term)
            else -> null
        }

    private fun operation(term: Struct): Linear? =
        when {
            term.arity == 1 && term.functor == "-" -> linear(term[0])?.scaled(-Rational.ONE)
            term.arity == 1 && term.functor == "+" -> linear(term[0])
            term.arity != 2 -> null
            term.functor == "+" -> linear(term[0])?.let { a -> linear(term[1])?.let { a.plus(it) } }
            term.functor == "-" -> linear(term[0])?.let { a -> linear(term[1])?.let { a.minus(it) } }
            term.functor == "*" -> {
                val a = linear(term[0])
                val b = linear(term[1])
                when {
                    a == null || b == null -> null
                    a.isConstant -> b.scaled(a.constant)
                    b.isConstant -> a.scaled(b.constant)
                    else -> null
                }
            }
            term.functor == "/" -> {
                val a = linear(term[0])
                val b = linear(term[1])
                if (a == null || b == null || !b.isConstant || b.constant.isZero) null else a.scaled(Rational.ONE / b.constant)
            }
            else -> null
        }
}
