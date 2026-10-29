package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.core.Atom
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertTrue

// The flags as the facts stating them: those of a builder written as those of the API are, and the text of the flags
// read as facts alone
class FlagsTest {
    @Test
    fun theValuesOfABuilderAreAtoms() {
        val fact = Flags.none().modulesPath("/tmp/Mods").toString()
        assertEquals("modulesPath('/tmp/Mods').\n", fact)
        val flags = Flags.of(FlagsBuilder(modulesPath = "/tmp/Mods")).toString()
        assertTrue(fact in flags, flags)
        // a value in upper case is an atom, not a variable
        val stated = FlagsBuilder.setupSolver(FlagsBuilder(modulesPath = "Mods")).first { it.head?.functor == "modulesPath" }
        assertEquals(Atom.of("Mods"), stated.head!![0])
    }

    @Test
    fun aBuilderStatesItsExtensionsLastFirst() {
        val flags = Flags.of(FlagsBuilder(graphExtensions = listOf("standardPref", "bp"))).toString()
        assertTrue(flags.indexOf("graphExtension(bp)") < flags.indexOf("graphExtension(standardPref)"), flags)
    }

    @Test
    fun theFlagsOfAParsedTextAreFacts() {
        assertEquals(
            "argumentLabellingMode(preferred).\nqueryMode.\n",
            Flags.parse("argumentLabellingMode(preferred). queryMode.").toString(),
        )
        assertFailsWith<Arg2pException> { Flags.parse(":- x.") }
        assertFailsWith<Arg2pException> { Flags.parse("queryMode :- true, fail.") }
    }
}
