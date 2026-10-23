package it.unibo.tuprolog.argumentation.core.system

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.channel.OutputChannel
import kotlin.test.Test
import kotlin.test.assertEquals

// A theory not defining the hooks of arg2p (conflict/2, sup/2, naturalTerms, ...) is solved with no warning
class SolverWarningsTest {
    @Test
    fun noWarningForTheHooksATheoryDoesNotDefine() {
        val warnings = mutableListOf<String>()
        val solver =
            Arg2pSolverFactory.default(theory = "r1 : a => b.\na1 :-> a.", settings = FlagsBuilder().queryMode(false).create())
        solver.setWarnings(OutputChannel.of { warnings += it.message.orEmpty() })
        solver.solve(Struct.parse("buildLabelSets(In, Out, Und)", solver.operators)).first()
        assertEquals(emptyList(), warnings)
    }
}
