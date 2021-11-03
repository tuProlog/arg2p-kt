package it.unibo.tuprolog.argumentation.core.libs.language

import it.unibo.tuprolog.argumentation.core.libs.sources.RuleTranslator

actual object RuleParser : RuleParserBase() {
    override val prologRawTheory: String
        get() = RuleTranslator.theoryCode
}
