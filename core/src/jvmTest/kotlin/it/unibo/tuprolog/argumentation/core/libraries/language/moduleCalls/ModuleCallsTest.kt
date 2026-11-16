package it.unibo.tuprolog.argumentation.core.libraries.language.moduleCalls

import com.sun.net.httpserver.HttpServer
import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.mining.graph
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
            settings = Flags.defaults().modulesPath(path).library(),
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
                .solve(Struct.parse("module::callModule(['greetings'], hello(X))", solver.operators))
                .first()

        assertTrue(solution.isYes, "callModule should solve a goal defined by the loaded module")
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
                .solve(Struct.parse("module::callModule(['greetings', 'farewells'], (hello(X), bye(Y)))", solver.operators))
                .first()

        assertTrue(solution.isYes, "callModule should solve a goal using the clauses of all the modules, got $solution")
    }

    @Test
    fun callModuleBindsTheAnonymousVariablesOfTheGoal() {
        val path = temporaryModule()
        val solver = solverWithModulesIn(path)

        val solution =
            solver
                .solve(Struct.parse("S = hello(_), module::callModule(['greetings'], S)", solver.operators))
                .first()

        assertTrue(solution.isYes, "callModule should solve a goal defined by the loaded module")
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
                .solve(Struct.parse("module::callModule(['theory'], arg2p::solve(d, Res))", solver.operators))
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
            .solve(Struct.parse("module::callModule(['theory'], arg2p::solve)", solver.operators))
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
                "module::callModule(['greetings'], hello(X))",
                "module::callModule(['$base/greetings.pl'], hello(X))",
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
                .solve(Struct.parse("module::callModule(['$path/greetings.pl'], hello(X))", solver.operators))
                .first()

        assertTrue(solution.isYes, "callModule should accept a fully qualified .pl path")
    }

    // The modules path as the platform writes it (C:\mods on Windows), with a directory in upper case
    @Test
    fun callModuleResolvesNamesAgainstAModulesPathAsThePlatformWritesIt() {
        val directory = modules("greetings.pl" to moduleTheory, directory = Files.createTempDirectory("arg2p").resolve("Mods"))
        val solution = solve(solverWithModulesIn(directory.toString()), "module::callModule(['greetings'], hello(X))")
        assertTrue(solution.isYes, "expected the module in $directory to be found, got $solution")
    }

    // A relative path ending in .pl, as a name, resolved against the modules path
    @Test
    fun callModuleResolvesRelativePathsAgainstModulesPath() {
        val directory = modules()
        Files.createDirectories(directory.resolve("cases"))
        Files.writeString(directory.resolve("cases/greetings.pl"), moduleTheory)
        val solver = solverWithModulesIn(directory.toString())
        listOf("'cases/greetings.pl'", "'cases/greetings'").forEach { module ->
            val solution = solve(solver, "module::callModule([$module], hello(X))")
            assertTrue(solution.isYes, "expected $module to be found in $directory, got $solution")
        }
    }

    @Test
    fun callModuleTakesANameContainingPlForAName() {
        val directory = modules("my.plan.pl" to moduleTheory)
        val solution = solve(solverWithModulesIn(directory.toString()), "module::callModule(['my.plan'], hello(X))")
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
        val solver = Arg2pSolverFactory.default(settings = Flags.defaults().library())
        listOf("'$relative/greetings'", "'$relative/greetings.pl'").forEach { module ->
            val solution = solve(solver, "module::callModule([$module], hello(X))")
            assertTrue(solution.isYes, "expected $module to be found in the working directory, got $solution")
        }
    }

    @Test
    fun callModuleReadsModulesCallingOtherModules() {
        val directory = modules("calls.pl" to "both(Y) :- utils::appendLists([[a], [b]], Y).\n")
        val solution = solve(solverWithModulesIn(directory.toString()), "module::callModule(['calls'], both(Y))")
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
        val solution = solve(solverWithModulesIn(directory.toString()), "module::callModule(['broken'], hello(X))")
        assertTrue(solution.exception is SyntaxError, "expected a syntax error, got $solution")
    }

    // The modules read as one text: an operator one declares holds in those after it
    @Test
    fun callModuleReadsTheOperatorsOfAModuleInTheNextOnes() {
        val directory = modules("ops.pl" to ":- op(720, xfy, and).\n", "facts.pl" to "both(a and b).\n")
        val solution = solve(solverWithModulesIn(directory.toString()), "module::callModule(['ops', 'facts'], both(X))")
        assertTrue(solution.isYes, "expected the operator of ops to hold in facts, got $solution")
    }

    // A syntax error reported at the module it is in, and at the line within that module
    @Test
    fun callModuleReportsASyntaxErrorAtItsModuleAndLine() {
        val directory = modules("good.pl" to "hello(world).\nhello(moon).\n", "bad.pl" to "ok(1).\nhello(.\n")
        val solution = solve(solverWithModulesIn(directory.toString()), "module::callModule(['good', 'bad'], hello(X))")
        val message = (solution.exception as? SyntaxError)?.message.orEmpty()
        assertTrue("bad.pl" in message && " at 2:" in message, "expected the error in bad.pl at line 2, got $message")
    }

    @Test
    fun callModuleRaisesAnExistenceErrorForAModuleThatCannotBeRead() {
        val solution = solve(solverWithModulesIn(temporaryModule()), "module::callModule(['missing'], hello(X))")
        assertTrue(solution.exception is ExistenceError, "expected an existence error, got $solution")
    }
}
