package it.unibo.tuprolog.argumentation.ui.gui

import it.unibo.tuprolog.argumentation.core.runtime.theory.RuleAnnotations
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.theory.Theory

/** [solver], loading the theory parsed from [text] with its clauses tagged with the identifiers of their comments */
internal class AnnotatedSolver(
    private val solver: MutableSolver,
    private val text: String,
) : MutableSolver by solver {
    override fun loadStaticKb(theory: Theory) = solver.loadStaticKb(RuleAnnotations.annotate(theory, text))
}
