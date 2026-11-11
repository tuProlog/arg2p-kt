package it.unibo.tuprolog.argumentation.federation

import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue

/** Sites federate across the patch releases of arg2p, not across its minor ones or another protocol of the frames */
class SiteVersionTest {
    private val minor = ARG2P_VERSION.split('.').take(2).joinToString(".")

    @Test
    fun theSameVersion() {
        assertTrue(Site.compatible(Site.VERSION))
    }

    @Test
    fun anotherPatchRelease() {
        assertTrue(Site.compatible("${Site.PROTOCOL} $minor.99"))
    }

    @Test
    fun anotherMinorRelease() {
        assertFalse(Site.compatible("${Site.PROTOCOL} 0.0.1"))
    }

    @Test
    fun anotherProtocol() {
        assertFalse(Site.compatible("arg2p-federation/2 $ARG2P_VERSION"))
        assertFalse(Site.compatible(""))
    }
}
