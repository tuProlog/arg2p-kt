package it.unibo.tuprolog.argumentation.core.libs.language

actual object RuleParser : RuleParserBase() {
    override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.RuleTranslator.theoryCode
}
