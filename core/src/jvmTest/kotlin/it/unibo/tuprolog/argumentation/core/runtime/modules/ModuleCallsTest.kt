package it.unibo.tuprolog.argumentation.core.runtime.modules

import com.sun.net.httpserver.HttpServer
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.model.mining.graph
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.exception.error.ExistenceError
import it.unibo.tuprolog.solve.exception.error.SyntaxError
import java.net.InetSocketAddress
import java.nio.file.Files
import java.nio.file.Path
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class ModuleCallsTest {
    private val moduleTheory = "hello(world).\n"

    private fun solverWithModulesIn(path: String): MutableSolver =
        Arg2pSolverFactory.default(
            theory = "",
            settings = FlagsBuilder(modulesPath = path).create(),
        )

    private fun temporaryModule(): String {
        val directory = Files.createTempDirectory("arg2p-modules")
        Files.writeString(directory.resolve("greetings.pl"), moduleTheory)
        return directory.toString().replace("\\", "/")
    }

    // The first solution of [goal] on [solver]
    private fun solve(
        solver: MutableSolver,
        goal: String,
    ) = solver.solve(Struct.parse(goal, solver.operators)).first()

    // A directory of modules, [modules] by their file name
    private fun modules(
        vararg modules: Pair<String, String>,
        directory: Path = Files.createTempDirectory("arg2p-modules"),
    ): Path =
        directory.also { Files.createDirectories(it) }.also { dir ->
            modules.forEach { (file, theory) -> Files.writeString(dir.resolve(file), theory) }
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
    fun callModuleLoadsEveryModuleGiven() {
        val path = temporaryModule()
        Files.writeString(Path.of(path, "farewells.pl"), "bye(moon).\n")
        val solver = solverWithModulesIn(path)

        val solution =
            solver
                .solve(Struct.parse("module::call_module(['greetings', 'farewells'], (hello(X), bye(Y)))", solver.operators))
                .first()

        assertTrue(solution.isYes, "call_module should solve a goal using the clauses of all the modules, got $solution")
    }

    @Test
    fun callModuleBindsTheAnonymousVariablesOfTheGoal() {
        val path = temporaryModule()
        val solver = solverWithModulesIn(path)

        val solution =
            solver
                .solve(Struct.parse("S = hello(_), module::call_module(['greetings'], S)", solver.operators))
                .first()

        assertTrue(solution.isYes, "call_module should solve a goal defined by the loaded module")
        val bound =
            solution.substitution.entries
                .first { it.key.name == "S" }
                .value
        assertEquals("hello(world)", bound.toString())
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
        // module stays minable from the calling solver: this is what the solvers of the playground draw.
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
    fun callModuleAcceptsAnExplicitFilePath() {
        val path = temporaryModule()
        val solver = solverWithModulesIn(path)

        val solution =
            solver
                .solve(Struct.parse("module::call_module(['$path/greetings.pl'], hello(X))", solver.operators))
                .first()

        assertTrue(solution.isYes, "call_module should accept a fully qualified .pl path")
    }

    // The modules path as the platform writes it (C:\mods on Windows), with a directory in upper case
    @Test
    fun callModuleResolvesNamesAgainstAModulesPathAsThePlatformWritesIt() {
        val directory = modules("greetings.pl" to moduleTheory, directory = Files.createTempDirectory("arg2p").resolve("Mods"))
        val solution = solve(solverWithModulesIn(directory.toString()), "module::call_module(['greetings'], hello(X))")
        assertTrue(solution.isYes, "expected the module in $directory to be found, got $solution")
    }

    @Test
    fun callModuleTakesANameContainingPlForAName() {
        val directory = modules("my.plan.pl" to moduleTheory)
        val solution = solve(solverWithModulesIn(directory.toString()), "module::call_module(['my.plan'], hello(X))")
        assertTrue(solution.isYes, "expected my.plan to be the name of a module, got $solution")
    }

    @Test
    fun callModuleResolvesNamesAndPathsAgainstTheWorkingDirectoryWithoutModulesPath() {
        val workingDirectory = Path.of("").toAbsolutePath()
        val directory =
            modules(
                "greetings.pl" to moduleTheory,
                directory =
                    Files.createTempDirectory(
                        workingDirectory.resolve("build").also {
                            Files.createDirectories(it)
                        },
                        "modules",
                    ),
            )
        val relative = workingDirectory.relativize(directory).toString().replace("\\", "/")
        val solver = Arg2pSolverFactory.default(settings = FlagsBuilder().create())
        listOf("'$relative/greetings'", "'$relative/greetings.pl'").forEach { module ->
            val solution = solve(solver, "module::call_module([$module], hello(X))")
            assertTrue(solution.isYes, "expected $module to be found in the working directory, got $solution")
        }
    }

    @Test
    fun callModuleReadsModulesCallingOtherModules() {
        val directory = modules("calls.pl" to "both(Y) :- utils::appendLists([[a], [b]], Y).\n")
        val solution = solve(solverWithModulesIn(directory.toString()), "module::call_module(['calls'], both(Y))")
        assertTrue(solution.isYes, "expected a module with :: to be read, got $solution")
        assertEquals(
            "[a, b]",
            solution.substitution.entries
                .first { it.key.name == "Y" }
                .value
                .toString(),
        )
    }

    @Test
    fun callModuleRaisesASyntaxErrorForAModuleThatDoesNotParse() {
        val directory = modules("broken.pl" to "hello(.\n")
        val solution = solve(solverWithModulesIn(directory.toString()), "module::call_module(['broken'], hello(X))")
        assertTrue(solution.exception is SyntaxError, "expected a syntax error, got $solution")
    }

    @Test
    fun callModuleRaisesAnExistenceErrorForAModuleThatCannotBeRead() {
        val solution = solve(solverWithModulesIn(temporaryModule()), "module::call_module(['missing'], hello(X))")
        assertTrue(solution.exception is ExistenceError, "expected an existence error, got $solution")
    }
}
