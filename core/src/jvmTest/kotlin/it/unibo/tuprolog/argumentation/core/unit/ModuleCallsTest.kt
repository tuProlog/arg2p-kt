package it.unibo.tuprolog.argumentation.core.unit

import com.sun.net.httpserver.HttpServer
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.libs.basic.FlagsBuilder
import it.unibo.tuprolog.argumentation.core.mining.graph
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.MutableSolver
import java.net.InetSocketAddress
import java.nio.file.Files
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class ModuleCallsTest {
    private val moduleTheory = "hello(world).\n"

    private fun solverWithModulesIn(path: String): MutableSolver =
        Arg2pSolverFactory.default(
            theory = "",
            settings = FlagsBuilder(modulesPath = "'$path'").create(),
        )

    private fun temporaryModule(): String {
        val directory = Files.createTempDirectory("arg2p-modules")
        Files.writeString(directory.resolve("greetings.pl"), moduleTheory)
        return directory.toString().replace("\\", "/")
    }

    @Test
    fun callModuleResolvesModuleNamesAgainstModulesPath() {
        val path = temporaryModule()
        val solver = solverWithModulesIn(path)

        val solution =
            solver
                .solve(Struct.parse("module::call_module(['greetings'], hello(X))", solver.operators))
                .first()

        assertTrue(solution.isYes, "call_module should solve a goal defined by the loaded module")
        assertTrue(
            solution.substitution.values.any { it.toString() == "world" },
            "expected X to be bound to world, got ${solution.substitution}",
        )
    }

    @Test
    fun callModuleEvaluatesAModuleWrittenInArg2pSyntax() {
        val directory = Files.createTempDirectory("arg2p-modules")
        Files.writeString(directory.resolve("theory.pl"), "f1 :=> d.\nf2 :=> -d.\nsup(f1, f2).\n")
        val solver = solverWithModulesIn(directory.toString().replace("\\", "/"))

        val solution =
            solver
                .solve(Struct.parse("module::call_module(['theory'], arg2p::solve(d, Res))", solver.operators))
                .first()

        assertTrue(solution.isYes, "a module written in arg2p syntax should be evaluated, got $solution")
        assertTrue(
            solution.substitution.values.any { it.toString() == "[in(d)]" },
            "expected d to be accepted thanks to sup(f1, f2), got ${solution.substitution}",
        )
    }

    @Test
    fun callModuleLeavesTheGraphAvailableForMining() {
        val directory = Files.createTempDirectory("arg2p-modules")
        Files.writeString(directory.resolve("theory.pl"), "f1 :=> d.\nf2 :=> -d.\nsup(f1, f2).\n")
        val solver = solverWithModulesIn(directory.toString().replace("\\", "/"))

        solver
            .solve(Struct.parse("module::call_module(['theory'], arg2p::solve)", solver.operators))
            .first()

        // The evaluation contexts live on the shared Context library, so the graph built inside the
        // module stays minable from the calling solver: this is what the IDE draws.
        val graph = solver.graph()
        assertEquals(2, graph.labellings.size, "expected both conclusions to be labelled, got $graph")
        assertEquals(1, graph.attacks.size, "expected the attack between the two arguments to survive")
    }

    @Test
    fun callModuleLoadsModulesOverHttp() {
        val server = HttpServer.create(InetSocketAddress("127.0.0.1", 0), 0)
        val body = moduleTheory.toByteArray()
        listOf("/greetings.pl", "/greetings").forEach { route ->
            server.createContext(route) { exchange ->
                exchange.sendResponseHeaders(200, body.size.toLong())
                exchange.responseBody.use { it.write(body) }
            }
        }
        server.start()
        val base = "http://127.0.0.1:${server.address.port}"

        try {
            val solver = solverWithModulesIn(base)
            // A module name resolved against a remote modulesPath, and a URL given explicitly.
            listOf(
                "module::call_module(['greetings'], hello(X))",
                "module::call_module(['$base/greetings.pl'], hello(X))",
            ).forEach { goal ->
                val solution = solver.solve(Struct.parse(goal, solver.operators)).first()
                assertTrue(solution.isYes, "expected $goal to load the remote module")
                assertTrue(
                    solution.substitution.values.any { it.toString() == "world" },
                    "expected X to be bound to world, got ${solution.substitution}",
                )
            }
        } finally {
            server.stop(0)
        }
    }

    @Test
    fun callModuleFailsWhenTheModuleCannotBeLoaded() {
        val path = temporaryModule()
        val solver = solverWithModulesIn(path)

        val solution =
            solver
                .solve(Struct.parse("module::call_module(['missing'], hello(X))", solver.operators))
                .first()

        // The primitive raises an existence error, which the :: dispatch turns into a plain failure.
        assertFalse(solution.isYes, "a module that cannot be loaded must not succeed")
    }

    @Test
    fun callModuleAcceptsAnExplicitFilePath() {
        val path = temporaryModule()
        val solver = solverWithModulesIn(path)

        val solution =
            solver
                .solve(Struct.parse("module::call_module(['$path/greetings.pl'], hello(X))", solver.operators))
                .first()

        assertTrue(solution.isYes, "call_module should accept a fully qualified .pl path")
    }
}
