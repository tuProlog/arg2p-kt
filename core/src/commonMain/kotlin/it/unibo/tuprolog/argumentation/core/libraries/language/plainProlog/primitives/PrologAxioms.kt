package it.unibo.tuprolog.argumentation.core.libraries.language.plainProlog.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.parser.helpers.RuleAnnotations
import it.unibo.tuprolog.argumentation.core.libraries.framework.parser.helpers.TheoryClauses
import it.unibo.tuprolog.argumentation.core.libraries.language.plainProlog.helpers.ClauseConversion
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.core.toTerm
import it.unibo.tuprolog.solve.Signature

/**
 * prologAxioms(-Axioms): the Prolog facts of the theory with no identifier (see [RuleAnnotations]) as axioms,
 * `[Id, Fact]`
 */
internal object PrologAxioms : AbstractNativePrimitive(Signature("prologAxioms", 1)) {
    override fun call(call: NativeCall): Int =
        TheoryClauses.reply(call, signature) { clauses, prologScope ->
            clauses
                .filter { (_, clause) -> ClauseConversion.isPrologFact(clause) && RuleAnnotations.of(clause) == null }
                .map { (index, clause) -> prologScope.logicListOf(ClauseConversion.ruleId(index), clause.head!!) }
                .toTerm()
        }
}
