@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.core.model.terms.TermValues
import kotlin.js.JsExport
import kotlin.js.JsName
import kotlin.js.JsStatic
import kotlin.jvm.JvmStatic
import it.unibo.tuprolog.core.Atom as LogicAtom
import it.unibo.tuprolog.core.Integer as LogicInteger
import it.unibo.tuprolog.core.List as LogicList
import it.unibo.tuprolog.core.Real as LogicReal
import it.unibo.tuprolog.core.Struct as LogicStruct
import it.unibo.tuprolog.core.Term as LogicTerm
import it.unibo.tuprolog.core.Var as LogicVar

private typealias ValueList = kotlin.collections.List<Value>

/**
 * A Prolog term, as arg2p gives it (a statement, the binding of a variable, ...) and takes it: an [Atom], an
 * [Integer], a [Real], a [Variable], a [List] or a compound term ([Struct]). A value is written as arg2p writes it,
 * with its operators (`-a`, `r1 : [a] => b`); [parse] reads it back.
 */
@JsExport
sealed class Value {
    /** What the value is: atom, integer, real, variable, list or struct */
    abstract val kind: String

    /** The value as Prolog text, written with the operators of arg2p */
    override fun toString(): String = TermValues.format(toTerm())

    /** The term of tuProlog of the value */
    internal abstract fun toTerm(): LogicTerm

    /** An atom, as `a` or `'Hello world'` */
    data class Atom(
        val name: String,
    ) : Value() {
        override val kind: String get() = "atom"

        override fun toString(): String = super.toString()

        override fun toTerm(): LogicTerm = LogicAtom.of(name)
    }

    /** An integer (one beyond the range of 32 bits is a [Real]) */
    data class Integer(
        val value: Int,
    ) : Value() {
        override val kind: String get() = "integer"

        override fun toString(): String = super.toString()

        override fun toTerm(): LogicTerm = LogicInteger.of(value)
    }

    /** A floating-point number */
    data class Real(
        val value: Double,
    ) : Value() {
        override val kind: String get() = "real"

        override fun toString(): String = super.toString()

        override fun toTerm(): LogicTerm = LogicReal.of(value)
    }

    /** A variable, by its name */
    data class Variable(
        val name: String,
    ) : Value() {
        override val kind: String get() = "variable"

        override fun toString(): String = name

        override fun toTerm(): LogicTerm = LogicVar.of(name)
    }

    /** A list, as `[a, b]` (`[]` the empty one) */
    data class List(
        @JsExport.Ignore @JsName("itemsList") val items: ValueList,
    ) : Value() {
        override val kind: String get() = "list"

        /** The items of the list (an array in JavaScript) */
        @JsName("items")
        val itemArray: Array<Value> get() = items.toTypedArray()

        override fun toString(): String = super.toString()

        override fun toTerm(): LogicTerm = LogicList.of(items.map { it.toTerm() })
    }

    /** A compound term, as `f(a, b)` or `-a` */
    data class Struct(
        val functor: String,
        @JsExport.Ignore @JsName("argsList") val args: ValueList,
    ) : Value() {
        override val kind: String get() = "struct"

        /** The arguments of the term (an array in JavaScript) */
        @JsName("args")
        val argArray: Array<Value> get() = args.toTypedArray()

        val arity: Int get() = args.size

        override fun toString(): String = super.toString()

        override fun toTerm(): LogicTerm = LogicStruct.of(functor, args.map { it.toTerm() })
    }

    companion object {
        /** The value written in [text], with the operators of arg2p: an [Arg2pException] telling where it does not parse otherwise */
        @JvmStatic
        @JsStatic
        fun parse(text: String): Value = TermValues.of(SyntaxProblem.parsing("Value") { TermValues.term(text) })

        /** A compound term of [functor] and [args] (an array in JavaScript) */
        @JvmStatic
        @JsStatic
        fun struct(
            functor: String,
            args: Array<Value>,
        ): Value = Struct(functor, args.toList())

        /** A list of [items] */
        @JvmStatic
        @JsStatic
        fun list(items: Array<Value>): Value = List(items.toList())
    }
}
