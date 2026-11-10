package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.constraints.helpers

import org.gciatto.kt.math.BigInteger

/** An exact rational number, in lowest terms with a positive denominator: the numbers the constraints are solved on */
internal class Rational private constructor(
    val numerator: BigInteger,
    val denominator: BigInteger,
) : Comparable<Rational> {
    val signum: Int get() = numerator.signum

    val isZero: Boolean get() = numerator.signum == 0

    operator fun plus(other: Rational): Rational =
        of(numerator * other.denominator + other.numerator * denominator, denominator * other.denominator)

    operator fun minus(other: Rational): Rational =
        of(numerator * other.denominator - other.numerator * denominator, denominator * other.denominator)

    operator fun times(other: Rational): Rational = of(numerator * other.numerator, denominator * other.denominator)

    operator fun div(other: Rational): Rational {
        require(!other.isZero) { "division by zero" }
        return of(numerator * other.denominator, denominator * other.numerator)
    }

    operator fun unaryMinus(): Rational = Rational(-numerator, denominator)

    override fun compareTo(other: Rational): Int = (numerator * other.denominator).compareTo(other.numerator * denominator)

    override fun equals(other: Any?): Boolean = other is Rational && numerator == other.numerator && denominator == other.denominator

    override fun hashCode(): Int = 31 * numerator.hashCode() + denominator.hashCode()

    override fun toString(): String = if (denominator == BigInteger.ONE) "$numerator" else "$numerator/$denominator"

    companion object {
        val ZERO = Rational(BigInteger.ZERO, BigInteger.ONE)
        val ONE = Rational(BigInteger.ONE, BigInteger.ONE)

        fun of(value: Long): Rational = Rational(BigInteger.of(value), BigInteger.ONE)

        fun of(value: BigInteger): Rational = Rational(value, BigInteger.ONE)

        fun of(
            numerator: BigInteger,
            denominator: BigInteger,
        ): Rational {
            require(denominator.signum != 0) { "zero denominator" }
            val divisor = numerator.gcd(denominator).let { if (denominator.signum < 0) -it else it }
            return if (divisor == BigInteger.ONE) Rational(numerator, denominator) else Rational(numerator / divisor, denominator / divisor)
        }

        /** The value of a decimal numeral, `61.167` or `1.5E-3`, exactly: a float read from a theory is the number written */
        fun ofDecimal(text: String): Rational {
            val exponentAt = text.indexOfFirst { it == 'e' || it == 'E' }
            val mantissa = if (exponentAt < 0) text else text.substring(0, exponentAt)
            val exponent = if (exponentAt < 0) 0 else text.substring(exponentAt + 1).removePrefix("+").toInt()
            val point = mantissa.indexOf('.')
            val digits = if (point < 0) mantissa else mantissa.removeRange(point, point + 1)
            val decimals = if (point < 0) 0 else mantissa.length - point - 1
            val scale = exponent - decimals
            val value = BigInteger.of(digits)
            return if (scale >= 0) of(value * BigInteger.TEN.pow(scale)) else of(value, BigInteger.TEN.pow(-scale))
        }
    }
}
