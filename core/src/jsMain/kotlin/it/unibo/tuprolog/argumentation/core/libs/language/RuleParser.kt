package it.unibo.tuprolog.argumentation.core.libs.language

actual object RuleParser : RuleParserBase() {
    actual override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.RuleTranslator.theoryCode
}
