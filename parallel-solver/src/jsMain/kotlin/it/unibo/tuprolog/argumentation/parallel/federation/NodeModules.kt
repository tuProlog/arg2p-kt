package it.unibo.tuprolog.argumentation.parallel.federation

/**
 * The module [name] of Node.js (or one of the packages of this one), loaded as this module would require it: when
 * needed alone, so that browsers, which have none of them, never load it
 */
internal fun nodeModule(name: String): dynamic = js("process.getBuiltinModule('module').createRequire(import.meta.url)(name)")

/** The URL of this module, as Node.js loads it */
internal fun thisModule(): String = js("import.meta.url") as String
