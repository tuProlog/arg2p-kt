package it.unibo.tuprolog.argumentation

import kotlin.js.JsExport
import kotlin.jvm.JvmOverloads

/**
 * An evaluation halted by an error: a theory its profile refuses, a goal that cannot be parsed, ... When a text does
 * not parse (a theory, a query, flags), [syntax] tells where.
 */
@JsExport
class Arg2pException
    @JvmOverloads
    constructor(
        message: String,
        val syntax: SyntaxProblem? = null,
    ) : Exception(message)
