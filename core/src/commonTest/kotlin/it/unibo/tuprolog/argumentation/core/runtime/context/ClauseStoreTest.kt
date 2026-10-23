package it.unibo.tuprolog.argumentation.core.runtime.context

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

class ClauseStoreTest {
    // An anonymous variable occurring twice (as the bindings of a `_` leave it) stays one variable
    @Test
    fun aSharedAnonymousVariableIsOneVariable() {
        val anonymous = Var.anonymous()
        val store = ClauseStore().also { it.assertA(Struct.of("u", anonymous, anonymous)) }
        assertEquals(0, store.solve(Struct.parse("u(a, b)")).count())
        assertEquals(1, store.solve(Struct.parse("u(a, a)")).count())
        store.retractAll(Struct.parse("u(a, b)"))
        assertEquals(1, store.solve(Struct.parse("u(X, Y)")).count())
    }
}
