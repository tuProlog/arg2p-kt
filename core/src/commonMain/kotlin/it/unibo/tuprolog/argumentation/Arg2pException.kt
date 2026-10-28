package it.unibo.tuprolog.argumentation

import kotlin.js.JsExport

/** An evaluation halted by an error: a theory its profile refuses, a goal that cannot be parsed, ... */
@JsExport
class Arg2pException(
    message: String,
) : Exception(message)
