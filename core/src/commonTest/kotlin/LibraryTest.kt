import it.unibo.argumentation.arg2p.Arg2pLibrary
import kotlin.test.Test
import kotlin.test.assertEquals

class LibraryTest {

    @Test
    fun loadLibraries() {
        val lib = Arg2pLibrary.get
        assertEquals(lib.size, 8)
    }
}