package it.unibo.tuprolog.argumentation.core.modules.extra

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import kotlin.test.Test
import kotlin.test.assertTrue

class InterpreterModuleTest {
    @Test
    fun aDisjunctionIsProvedByEitherSide() {
        val module = ModuleTesting()
        assertTrue(module.holds("interpreter::solve((true ; fail), _)"))
        assertTrue(module.holds("interpreter::solve((fail ; true), _)"))
    }
}
