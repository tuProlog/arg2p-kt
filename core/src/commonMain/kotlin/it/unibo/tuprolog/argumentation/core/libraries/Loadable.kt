package it.unibo.tuprolog.argumentation.core.libraries

import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.library.Library

/** A module, loaded by its calls, which name it by its [identifier] */
interface Loadable : ArgLibrary {
    fun identifier(): String

    /** The flag making the library read part of a theory (see parser::extension/1), if it is the flag that does */
    val parsingFlag: ArgsFlag?
        get() = null

    /** The operators the theories this module reads are written with (`r : a => b` for the parser), none by default */
    val notation: OperatorSet
        get() = OperatorSet.EMPTY

    /** The predicates the module defines, its clauses and its primitives, as name/arity (see module_defines/2) */
    val definitions: Set<String>
        get() = definitionsOf(baseContent)
}

/** The predicates [library] defines, its clauses and its primitives, as name/arity */
internal fun definitionsOf(library: Library): Set<String> =
    (
        library.clauses.mapNotNull { clause -> clause.head?.let { "${it.functor}/${it.arity}" } } +
            library.primitives.keys.map { "${it.name}/${it.arity}" }
    ).toSet()
