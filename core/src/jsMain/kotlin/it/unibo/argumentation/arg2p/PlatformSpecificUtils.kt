package it.unibo.argumentation.arg2p

import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

external fun require(name: String): dynamic
external val __dirname: dynamic

val fs = require("fs")
val path = require("path");

actual fun loadTheoryFromFile(fileName: String): Theory {
    val completePath = path.join(
        __dirname,
        fileName
    )
    return Theory.parse(fs.readFileSync(completePath, "utf8") as String)
}