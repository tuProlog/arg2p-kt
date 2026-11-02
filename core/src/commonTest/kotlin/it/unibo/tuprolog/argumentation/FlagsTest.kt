package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.core.Atom
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertTrue

// The flags as the facts stating them, and the text of the flags read as facts alone
class FlagsTest {
    @Test
    fun theValuesAreAtoms() {
        assertEquals("modulesPath('/tmp/Mods').\n", Flags.none().modulesPath("/tmp/Mods").toString())
        // a value in upper case is an atom, not a variable
        val stated =
            Flags
                .none()
                .modulesPath("Mods")
                .library()
                .baseContent.clauses
                .first { it.head?.functor == "modulesPath" }
        assertEquals(Atom.of("Mods"), stated.head!![0])
    }

    @Test
    fun theExtensionsAreStatedInOrder() {
        val flags = Flags.defaults().graphExtensions(GraphExtension.STANDARD_PREF, GraphExtension.BP).toString()
        assertTrue(flags.indexOf("graphExtension(standardPref)") < flags.indexOf("graphExtension(bp)"), flags)
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
