package it.unibo.tuprolog.argumentation.core.libraries.language.plainProlog.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.parser.helpers.RuleAnnotations
import it.unibo.tuprolog.argumentation.core.libraries.framework.parser.helpers.TheoryClauses
import it.unibo.tuprolog.argumentation.core.libraries.language.plainProlog.helpers.ClauseConversion
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.toTerm
import it.unibo.tuprolog.solve.Signature

/**
 * prologPremises(-Premises): the Prolog facts of the theory with an identifier (see [RuleAnnotations]) as
 * defeasible premises, `[Id, Fact]`
 */
internal object PrologPremises : AbstractNativePrimitive(Signature("prologPremises", 1)) {
    override fun call(call: NativeCall): Int =
        TheoryClauses.reply(call, signature) { clauses, prologScope ->
            clauses
                .filter { (_, clause) -> ClauseConversion.isPrologFact(clause) }
                .mapNotNull { (_, clause) ->
                    RuleAnnotations.of(clause)?.let { prologScope.logicListOf(Atom.of(it), clause.head!!) }
                }.toTerm()
        }
}
