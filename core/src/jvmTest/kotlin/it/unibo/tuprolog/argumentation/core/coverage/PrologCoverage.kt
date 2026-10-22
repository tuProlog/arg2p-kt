package it.unibo.tuprolog.argumentation.core.coverage

import it.unibo.tuprolog.argumentation.core.engine.ClauseObservers
import it.unibo.tuprolog.argumentation.core.libs.Arg2pModules
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.core.Clause
import org.junit.runner.JUnitCore
import java.io.File
import java.util.Collections
import java.util.IdentityHashMap
import kotlin.test.Test

/**
 * The Prolog coverage of the tests: runs the test classes of arg2p and reports, for each module, the clauses the
 * engine never resolved a goal with (see [ClauseObservers]). Only run through jvmPrologCoverage; the report is written
 * to build/reports/prolog-coverage/coverage.md (or to the file of the property coverage.output).
 */
class PrologCoverage {
    // A clause of a module: the module, and the place of the clause among those of its predicate
    private class Owned(
        val module: String,
        val predicate: String,
        val index: Int,
        val clause: Clause,
    )

    private val excludedPackages = listOf("benchmark", "coverage", "snapshot")

    @Test
    fun coverage() {
        val owners = moduleClauses()
        val used = Collections.newSetFromMap(Collections.synchronizedMap(IdentityHashMap<Clause, Boolean>()))
        val result =
            ClauseObservers.register { used.add(it) }.use {
                JUnitCore.runClasses(*testClasses().toTypedArray())
            }
        val output = File(System.getProperty("coverage.output") ?: "build/reports/prolog-coverage/coverage.md")
        output.parentFile.mkdirs()
        output.writeText(report(owners, used, result.runCount, result.failureCount))
        println("PROLOG COVERAGE|${output.absolutePath}")
    }

    // The clauses of the modules, by identity: the engine resolves goals with the clauses of their libraries as they are
    private fun moduleClauses(): List<Owned> =
        Arg2pModules.all.filterIsInstance<Loadable>().distinctBy { it.identifier() }.flatMap { module ->
            val clauses = (module as it.unibo.tuprolog.argumentation.core.libs.ArgLibrary).content().clauses
            clauses
                .groupBy { "${it.head?.functor}/${it.head?.arity}" }
                .flatMap { (predicate, sameName) ->
                    sameName.mapIndexed { index, clause -> Owned(module.identifier(), predicate, index + 1, clause) }
                }
        }

    // The test classes on the class path, but for the tools
    private fun testClasses(): List<Class<*>> {
        val root = "it/unibo/tuprolog/argumentation/core"
        return System
            .getProperty("java.class.path")
            .split(File.pathSeparator)
            .map(::File)
            .filter { File(it, root).isDirectory }
            .flatMap { directory ->
                File(directory, root)
                    .walkTopDown()
                    .filter { it.isFile && it.name.endsWith("Test.class") && '$' !in it.name }
                    .map { it.relativeTo(directory).path.className() }
                    .toList()
            }.filter { name -> excludedPackages.none { ".$it." in name } }
            .distinct()
            .sorted()
            .map { Class.forName(it) }
    }

    private fun report(
        owners: List<Owned>,
        used: Set<Clause>,
        tests: Int,
        failures: Int,
    ): String =
        buildString {
            val byModule = owners.groupBy { it.module }
            val total = owners.count { it.clause in used }
            appendLine("# Prolog coverage of the tests")
            appendLine()
            appendLine("$tests tests run, $failures failed. A clause is covered when a goal is resolved with it.")
            appendLine()
            appendLine("**Total: $total of ${owners.size} clauses (${percent(total, owners.size)})**")
            appendLine()
            appendLine("| Module | Covered | Clauses | % |")
            appendLine("|---|---:|---:|---:|")
            val coverage = byModule.mapValues { (_, clauses) -> clauses.count { it.clause in used } }
            byModule.keys.sortedBy { coverage.getValue(it).toDouble() / byModule.getValue(it).size }.forEach { module ->
                val clauses = byModule.getValue(module).size
                appendLine("| $module | ${coverage.getValue(module)} | $clauses | ${percent(coverage.getValue(module), clauses)} |")
            }
            appendLine()
            appendLine("## Clauses never used")
            byModule.forEach { (module, clauses) ->
                val missed = clauses.filter { it.clause !in used }
                if (missed.isNotEmpty()) {
                    appendLine()
                    appendLine("### $module")
                    appendLine()
                    missed.forEach { appendLine("- `${it.predicate}` clause ${it.index}: `${it.clause.head}`") }
                }
            }
        }

    // The name of the class compiled to the file at this path
    private fun String.className() = removeSuffix(".class").replace(File.separatorChar, '.')

    private fun percent(
        part: Int,
        whole: Int,
    ) = if (whole == 0) "-" else "${100 * part / whole}%"
}
