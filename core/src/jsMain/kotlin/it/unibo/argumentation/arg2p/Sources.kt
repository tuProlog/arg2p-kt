package it.unibo.argumentation.arg2p

import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

actual object Sources {
    actual val argumentationEngineInterface: Theory
        get() = Theory.parse(ArgumentationEngineInterface.theoryCode)

    actual val abstractMode: Theory
        get() = Theory.parse(AbstractMode.theoryCode)

    actual val queryMode: Theory
        get() = Theory.parse(QueryMode.theoryCode)

    actual val argumentationGraph: Theory
        get() = Theory.parse(ArgumentationGraph.theoryCode)

    actual val prefArgumentationGraph: Theory
        get() = Theory.parse(PrefArgumentationGraph.theoryCode)

    actual val bpLabelling: Theory
        get() = Theory.parse(BpPartialComplete.theoryCode)

    actual val groundedLabelling: Theory
        get() = Theory.parse(Grounded.theoryCode)

    actual val completeLabelling: Theory
        get() = Theory.parse(Complete.theoryCode)

    actual val statementLabelling: Theory
        get() = Theory.parse(StatementLabelling.theoryCode)

    actual val ruleTranslator: Theory
        get() = Theory.parse(RuleTranslator.theoryCode)

    actual val debug: Theory
        get() = Theory.parse(Debug.theoryCode)

    actual val utils: Theory
        get() = Theory.parse(Utils.theoryCode)
}