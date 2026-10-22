package it.unibo.tuprolog.argumentation.core.docs

import it.unibo.tuprolog.argumentation.core.mining.arguments

import it.unibo.tuprolog.argumentation.core.runtime.modules.Module

import java.io.File

/**
 * The documentation of the Prolog modules of arg2p, from their sources: for each module (a PrologModule object) its
 * KDoc, the predicates documented in its Prolog text (`%! signature` followed by `%` lines), the modules it calls and
 * those calling it. The predicates other modules call, or the engine dispatches to, are listed when undocumented.
 *
 * Run through prologDocs: `main(sources, output)` writes an index and a page for each module to the output directory.
 */
fun main(arguments: Array<String>) {
    val sources = File(arguments.getOrElse(0) { "src/commonMain/kotlin" })
    val output = File(arguments.getOrElse(1) { "build/docs/prolog-modules" })
    val modules = PrologSources.modules(sources)
    output.mkdirs()
    val pages = ModulePages(modules)
    output.resolve("index.md").writeText(pages.index())
    modules.forEach { output.resolve("${it.identifier}.md").writeText(pages.page(it)) }
    println("PROLOG DOCS|${modules.size} modules|${output.absolutePath}")
}

/** A predicate of a module, with its documentation if it has one */
class Predicate(
    val name: String,
    val arity: Int,
    val signature: String? = null,
    val description: String = "",
) {
    val indicator: String
        get() = "$name/$arity"
}

/** A Prolog module of arg2p, as its source declares it */
class PrologModuleSource(
    val identifier: String,
    val objectName: String,
    val file: File,
    val description: String,
    val predicates: List<Predicate>,
    // The predicates of other modules it calls, by module
    val calls: Map<String, Set<String>>,
)

/** Reads the modules from the Kotlin sources */
object PrologSources {
    // A declaration of a module, with its KDoc: the name of the class or object, and the identifier of the module
    private val declaration =
        Regex("""(/\*\*(?:(?!\*/)[\s\S])*\*/\s*)?(?:object|abstract class) (\w+)\s*:\s*PrologModule\(\s*"[^"]+",\s*"([^"]+)"""")
    private val subclass = Regex("""(/\*\*(?:(?!\*/)[\s\S])*\*/\s*)?object (\w+)\s*:\s*(\w+)\(\)""")
    private val theory = Regex("""prologRawTheory: String\s*=\s*""${'"'}([\s\S]*?)""${'"'}\.trimIndent\(\)""")
    private val call = Regex("""(?<![\w:])([a-z]\w*):::?([a-z]\w*)(\()?""")

    fun modules(sources: File): List<PrologModuleSource> =
        sources
            .walkTopDown()
            .filter { it.isFile && it.extension == "kt" }
            .flatMap { file -> modulesIn(file) }
            .sortedBy { it.identifier }
            .toList()

    private fun modulesIn(file: File): List<PrologModuleSource> {
        val text = file.readText()
        val prolog =
            theory
                .find(text)
                ?.groupValues
                ?.get(1)
                ?.trimIndent() ?: return emptyList()
        return declaration.findAll(text).toList().map { found ->
            // a base class declaring the module, its object (and documentation) after it
            val concrete = subclass.findAll(text).firstOrNull { it.groupValues[3] == found.groupValues[2] }
            PrologModuleSource(
                identifier = found.groupValues[3],
                objectName = concrete?.groupValues?.get(2) ?: found.groupValues[2],
                file = file,
                description = kdoc(concrete?.groupValues?.get(1)?.ifBlank { null } ?: found.groupValues[1]),
                predicates = predicates(prolog),
                calls = calls(prolog),
            )
        }
    }

    private fun kdoc(comment: String): String =
        comment
            .trim()
            .removePrefix("/**")
            .removeSuffix("*/")
            .lines()
            .joinToString("\n") { it.trim().removePrefix("*").removePrefix(" ") }
            .trim()

    // The predicates defined in the text, in order, each with the documentation just before its first clause
    private fun predicates(prolog: String): List<Predicate> {
        val lines = prolog.lines()
        val defined = LinkedHashMap<String, Predicate>()
        var documented: Pair<String, StringBuilder>? = null
        for (line in lines) {
            when {
                line.startsWith("%! ") -> documented = line.removePrefix("%! ").trim() to StringBuilder()
                line.startsWith("%") && documented != null ->
                    documented.second.append(line.removePrefix("%").trim()).append(' ')
                line.isNotBlank() && line[0].isLowerCase() -> {
                    val head = head(line)
                    if (head != null && head.indicator !in defined) {
                        val doc = documented?.takeIf { (signature, _) -> head(signature)?.indicator == head.indicator }
                        defined[head.indicator] = Predicate(head.name, head.arity, doc?.first, doc?.second?.toString()?.trim() ?: "")
                    }
                    documented = null
                }
                line.isBlank() -> Unit
                else -> if (!line.startsWith("%")) documented = null
            }
        }
        return defined.values.toList()
    }

    // The name and arity of the head starting the line
    private fun head(line: String): Predicate? {
        val name = Regex("""^([a-z]\w*)""").find(line)?.value ?: return null
        val rest = line.substring(name.length)
        if (!rest.startsWith("(")) return Predicate(name, 0)
        return Predicate(name, arguments(rest) ?: return null)
    }

    // The number of the arguments in the parenthesis opening the text, if it closes on the same line
    private fun arguments(text: String): Int? {
        var depth = 0
        var count = 1
        var quoted = false
        for (char in text) {
            when {
                char == '\'' -> quoted = !quoted
                quoted -> Unit
                char == '(' || char == '[' -> depth++
                char == ')' || char == ']' -> {
                    depth--
                    if (depth == 0) return count
                }
                char == ',' && depth == 1 -> count++
            }
        }
        return null
    }

    private fun calls(prolog: String): Map<String, Set<String>> =
        prolog
            .lines()
            .filterNot { it.trimStart().startsWith("%") }
            .flatMap { line ->
                call.findAll(line).map { match ->
                    val arity = if (match.groupValues[3].isEmpty()) 0 else arguments(line.substring(match.range.last)) ?: -1
                    match.groupValues[1] to "${match.groupValues[2]}/$arity"
                }
            }.filter { (module, _) -> module[0].isLowerCase() && module != "context" }
            .groupBy({ it.first }, { it.second })
            .mapValues { (_, predicates) -> predicates.toSortedSet() }
}

/** The pages of the documentation */
class ModulePages(
    private val modules: List<PrologModuleSource>,
) {
    private val byIdentifier = modules.associateBy { it.identifier }

    // The predicates the engine of arg2p calls in the modules a flag selects
    private val dispatched =
        setOf(
            "buildArgumentationGraph/0",
            "buildAdditionalArguments/0",
            "buildAttacks/0",
            "modifyArgumentationGraph/0",
            "argumentLabelling/0",
            "statementLabelling/0",
            "locality/1",
            "queryDependencies/2",
            "queryDependants/2",
        )

    private fun callers(module: PrologModuleSource) = modules.filter { module.identifier in it.calls && it !== module }

    // The predicates of the module others call or the engine dispatches to; all those of the entry points of arg2p
    private fun exported(module: PrologModuleSource): List<Predicate> {
        if (module.identifier == ENTRY_POINTS) return module.predicates
        val called = modules.filter { it !== module }.flatMap { it.calls[module.identifier].orEmpty() }.toSet()
        return module.predicates.filter { it.indicator in called || it.indicator in dispatched }
    }

    private fun summary(module: PrologModuleSource) =
        module.description
            .substringBefore("\n\n")
            .replace("\n", " ")
            .substringBefore(". ")
            .removeSuffix(".")

    fun index(): String =
        buildString {
            appendLine("# The Prolog modules of arg2p")
            appendLine()
            appendLine("Generated from the sources by prologDocs. A module is called as `module::goal` (`module:::goal` in a new")
            appendLine("context branch); each page lists its documented predicates, the modules it calls and those calling it.")
            appendLine()
            appendLine("| Module | Kotlin object | Documented | Exported undocumented | |")
            appendLine("|---|---|---:|---:|---|")
            modules.forEach { module ->
                val undocumented = exported(module).count { it.signature == null }
                val documented = module.predicates.count { it.signature != null }
                val name = "[${module.identifier}](${module.identifier}.md)"
                appendLine("| $name | ${module.objectName} | $documented | $undocumented | ${summary(module)} |")
            }
        }

    fun page(module: PrologModuleSource): String =
        buildString {
            appendLine("# ${module.identifier}")
            appendLine()
            appendLine("Kotlin object `${module.objectName}`, in `${module.file.name}`.")
            appendLine()
            if (module.description.isNotBlank()) {
                appendLine(module.description)
                appendLine()
            }
            val uses =
                module.calls.keys
                    .filter { it != module.identifier }
                    .sorted()
            if (uses.isNotEmpty()) appendLine("**Uses:** " + uses.joinToString(", ") { link(it) } + "  ")
            val callers = callers(module).map { it.identifier }
            if (callers.isNotEmpty()) appendLine("**Used by:** " + callers.joinToString(", ") { link(it) })
            appendLine()
            appendLine("## Predicates")
            module.predicates.filter { it.signature != null }.forEach { predicate ->
                appendLine()
                appendLine("### `${predicate.signature}`")
                appendLine()
                appendLine(predicate.description)
            }
            val undocumented = exported(module).filter { it.signature == null }
            if (undocumented.isNotEmpty()) {
                appendLine()
                appendLine("## Undocumented")
                appendLine()
                appendLine("Called by other modules or dispatched to, without a `%!` comment:")
                appendLine()
                undocumented.forEach { appendLine("- `${it.indicator}`") }
            }
        }

    private fun link(identifier: String) = if (identifier in byIdentifier) "[$identifier]($identifier.md)" else "`$identifier`"

    private companion object {
        // The module of the entry points of arg2p, called by its users
        const val ENTRY_POINTS = "arg2p"
    }
}
