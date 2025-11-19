package it.unibo.tuprolog.argumentation.core.libs.language

import it.unibo.tuprolog.argumentation.core.TheoryUtils

actual object RuleParser : RuleParserBase() {
    actual override val prologRawTheory: String
        get() = TheoryUtils.loadTheoryFromPrologFile("core/translation/ruleTranslator")
}
