package it.unibo.tuprolog.argumentation.core.modules

import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.theory.Theory

/** The clauses of the theory, as a library of [alias] */
fun Theory.toLibrary(alias: String): Library = Library.of(alias, clauses = this.clauses)

/** The clauses of the library, as a theory */
fun Library.toTheory(): Theory = Theory.of(this.clauses)
