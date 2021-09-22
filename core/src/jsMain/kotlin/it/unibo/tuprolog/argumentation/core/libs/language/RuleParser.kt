package it.unibo.tuprolog.argumentation.core.libs.language

import it.unibo.tuprolog.argumentation.core.libs.sources.RuleTranslator
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

actual object RuleParser: RuleParserBase() {
    override val prologRawTheory: String
        get() = RuleTranslator.theoryCode
}
