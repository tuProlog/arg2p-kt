package it.unibo.tuprolog.argumentation.core.libraries.framework.loader

import it.unibo.tuprolog.argumentation.core.LibraryTesting
import kotlin.test.Test
import kotlin.test.assertEquals

class ModuleBuiltinsTest {
    private val module = LibraryTesting()

    @Test
    fun modulesSeeTheBuiltin() {
        // a module solves its goals with the libraries of the solver
        assertEquals("2", module.value("utils::length([a, b], N)", "N"))
    }
}
