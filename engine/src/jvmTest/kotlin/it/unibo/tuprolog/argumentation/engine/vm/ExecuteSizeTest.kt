package it.unibo.tuprolog.argumentation.engine.vm

import java.io.PrintWriter
import java.io.StringWriter
import java.nio.file.Paths
import java.util.spi.ToolProvider
import kotlin.test.Test
import kotlin.test.assertTrue

class ExecuteSizeTest {
    // Over 8000 bytes of bytecode HotSpot compiles a method no more (HugeMethodLimit): the machine runs ten times slower,
    // with nothing else to tell
    @Test
    fun theLoopOfTheMachineStaysCompiled() {
        val javap = ToolProvider.findFirst("javap").orElse(null) ?: return
        val classes =
            Paths
                .get(
                    Machine::class.java.protectionDomain.codeSource.location
                        .toURI(),
                ).toString()
        val out = StringWriter()
        val writer = PrintWriter(out)
        javap.run(writer, writer, "-c", "-p", "-cp", classes, Machine::class.java.name)
        val lines = out.toString().lines()
        val from = lines.indexOfFirst { it.contains(" execute(int);") }
        assertTrue(from >= 0, "no execute in Machine")
        val last =
            lines
                .drop(from + 1)
                .takeWhile { it.isNotBlank() }
                .mapNotNull {
                    Regex("""^\s*(\d+): \w""")
                        .find(it)
                        ?.groupValues
                        ?.get(1)
                        ?.toInt()
                }.last()
        assertTrue(last < 8000, "Machine.execute is $last bytes: over 8000 it is not compiled")
    }
}
