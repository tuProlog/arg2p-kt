package it.unibo.tuprolog.argumentation.core.loading

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import kotlin.test.Test
import kotlin.test.assertEquals

class ModuleBuiltinsTest {
    private val module = ModuleTesting()

    @Test
    fun modulesSeeTheBuiltin() {
        // a module solves its goals with the libraries of the solver
        assertEquals("2", module.value("utils::length([a, b], N)", "N"))
    }
}
