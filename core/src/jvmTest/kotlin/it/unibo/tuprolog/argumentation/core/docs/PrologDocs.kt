package it.unibo.tuprolog.argumentation.core.docs

import it.unibo.tuprolog.argumentation.core.modules.ArgLibrary
import it.unibo.tuprolog.argumentation.core.modules.LazyRawPrologContent
import it.unibo.tuprolog.argumentation.core.modules.Loadable
import it.unibo.tuprolog.argumentation.core.modules.ModuleRegistry
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.extractSignature
import java.io.File

/**
 * The reference of the Prolog modules of arg2p, as pages of the documentation site: an index, a section for each area
 * of the modules and a page for each module, with its KDoc, its predicates (the `%! signature` comments of its theory, followed by `%` lines), its
 * primitives, the flags it declares, the modules it calls and those calling it. The modules, their clauses and their
 * calls are read as the solvers load them (ModuleRegistry); only the KDoc comes from the Kotlin sources.
 *
 * Run through prologDocs: `main(sources, output)` writes the pages to the output directory.
 */
fun main(arguments: Array<String>) {
    val sources = File(arguments.getOrElse(0) { "src/commonMain/kotlin" })
    val output = File(arguments.getOrElse(1) { "build/docs/prolog-modules" })
    val reference = PrologReference(KDocs(sources))
    output.deleteRecursively()
    output.mkdirs()
    reference.pages().forEach { (path, page) -> output.resolve(path).also { it.parentFile.mkdirs() }.writeText(page) }
    println("PROLOG DOCS|${reference.modules.size} modules|${output.absolutePath}")
}

/** A predicate as its `%!` comment documents it: its signature, with the modes of the arguments, and its description */
class Documentation(
    val signature: String,
    val indicator: Signature,
    val description: String,
)

/** A module of arg2p, with what its documentation shows */
class ModuleDoc(
    val module: ArgLibrary,
    val area: Area,
    kdocs: KDocs,
) {
    val description: String = kdocs.of(module::class.simpleName!!)

    val identifier: String = (module as Loadable).identifier()
    val objectName: String = module::class.simpleName!!

    private val clauses = module.content().clauses.filterIsInstance<Clause>()

    /** The predicates it defines, in the order of its theory, and its primitives */
    val defined: Set<Signature> =
        clauses.map { it.head!!.extractSignature() }.toCollection(LinkedHashSet()) + module.content().primitives.keys

    /** Its native primitives, each with its KDoc */
    val primitives: Map<Signature, String> =
        module.content().primitives.mapValues { (signature, primitive) -> kdocs.ofPrimitive(signature, primitive) }

    /** The predicates its comments document, in their order */
    val documented: List<Documentation> =
        (module as? LazyRawPrologContent)?.let { Comments.of(it.prologRawTheory, module) }.orEmpty()

    /** Its predicates: those its comments document, then its native primitives no comment documents */
    val entries: List<Documentation> =
        documented +
            primitives
                .filterKeys { primitive -> documented.none { it.indicator == primitive } }
                .map { (signature, kdoc) -> Comments.ofPrimitive(signature, kdoc, module) }

    /** The predicates of other modules it calls, by module */
    val calls: Map<String, Set<Signature>> =
        clauses
            .flatMap { moduleCalls(it.body).toList() }
            .filter { (name, _) -> name != identifier }
            .groupBy({ it.first }, { it.second.extractSignature() })
            .mapValues { (_, signatures) -> signatures.toSet() }

    // The module calls in a term, at any depth, naming their module and goal
    private fun moduleCalls(term: Term): Sequence<Pair<String, Struct>> =
        when {
            term !is Struct -> emptySequence()
            term.arity == 2 && term.functor in setOf("::", ":::") && term[0].isAtom && term[1] is Struct ->
                sequenceOf(term[0].castToAtom().value to term[1].castToStruct()) + moduleCalls(term[1])
            else -> term.args.asSequence().flatMap { moduleCalls(it) }
        }
}

/** The `%!` comments of a theory */
object Comments {
    // The modes of the arguments in a signature (+In, -Out, ?Either, @Unchanged), removed to parse it
    private val modes = Regex("""([(,]\s*)[+\-?@]""")

    fun of(
        text: String,
        module: ArgLibrary,
    ): List<Documentation> {
        val found = mutableListOf<Documentation>()
        var signature: String? = null
        val description = mutableListOf<String>()

        fun close() {
            signature?.let { found += Documentation(it, indicator(it, module), paragraphs(description)) }
            signature = null
            description.clear()
        }
        text.trimIndent().lines().map { it.trim() }.forEach { line ->
            when {
                line.startsWith("%!") -> {
                    close()
                    signature = line.removePrefix("%!").trim()
                }
                line.startsWith("%") && signature != null -> description += line.removePrefix("%").trim()
                else -> close()
            }
        }
        close()
        return found
    }

    /**
     * A primitive as its KDoc documents it: the KDoc starts with its signature (`name(+Arg, ...): ...`), or documents
     * each of its arities that way, one after the other
     */
    fun ofPrimitive(
        signature: Signature,
        kdoc: String,
        module: ArgLibrary,
    ): Documentation {
        // the KDoc, split where a line starts with the name of the primitive
        val entries =
            kdoc.lines().fold(mutableListOf<String>()) { entries, line ->
                if (entries.isEmpty() || line.startsWith("${signature.name}(") || line.startsWith("${signature.name}:")) {
                    entries += line
                } else {
                    entries[entries.lastIndex] += " $line"
                }
                entries
            }
        for (entry in entries) {
            val end = if (entry.startsWith("${signature.name}(")) entry.indexOf("): ") + 1 else entry.indexOf(": ")
            if (end <= 0) continue
            val leading = entry.substring(0, end)
            if (runCatching { indicator(leading, module) }.getOrNull() == signature) {
                return Documentation(leading, signature, entry.substring(end + 2).replaceFirstChar { it.uppercase() })
            }
        }
        return Documentation("${signature.name}/${signature.arity}", signature, kdoc.replace('\n', ' '))
    }

    // The name and arity of a signature, parsed as a term
    private fun indicator(
        signature: String,
        module: ArgLibrary,
    ): Signature = Struct.parse(signature.replace(modes, "$1"), module.theoryOperators).extractSignature()

    // The lines of a comment as text: a line starting with "- " starts an item of a list
    private fun paragraphs(lines: List<String>): String =
        lines
            .fold(StringBuilder()) { text, line ->
                when {
                    line.startsWith("- ") -> text.append("\n").append(line)
                    line.isEmpty() -> text.append("\n\n")
                    text.isEmpty() || text.endsWith("\n") -> text.append(line)
                    else -> text.append(" ").append(line)
                }
            }.toString()
            .trim()
}

/** The KDoc of the declarations of the Kotlin sources, by the name of the declaration */
class KDocs(
    sources: File,
) {
    private val text = sources.walkTopDown().filter { it.isFile && it.extension == "kt" }.joinToString("\n") { it.readText() }

    private val link = Regex("""\[([\w.]+)]""")

    fun of(name: String): String {
        val own = documentation(Regex("""(/\*\*(?:(?!\*/)[\s\S])*\*/)\s*(?:internal\s+)?(?:object|class) $name\b"""))
        if (own != null) return own
        // an object of a base class declaring the module, documented on the base class
        val base = Regex("""object $name\s*:\s*(\w+)\(""").find(text)?.groupValues?.get(1) ?: return ""
        if (base == "PrologModule") return ""
        return documentation(Regex("""(/\*\*(?:(?!\*/)[\s\S])*\*/)\s*abstract class $base\b""")) ?: ""
    }

    /** The KDoc of a primitive: of its class, or the one starting with its name (`name(+Arg, ...): ...`) */
    fun ofPrimitive(
        signature: Signature,
        primitive: Any,
    ): String =
        primitive::class.simpleName?.let { of(it) }?.ifBlank { null }
            ?: documentation(Regex("""(/\*\*\s*\*\s*${Regex.escape(signature.name)}[(:](?:(?!\*/)[\s\S])*\*/)""")).orEmpty()

    private fun documentation(declaration: Regex): String? =
        declaration.find(text)?.groupValues?.get(1)?.let { comment ->
            comment
                .removePrefix("/**")
                .removeSuffix("*/")
                .lines()
                .joinToString("\n") { it.trim().removePrefix("*").removePrefix(" ") }
                .trim()
                // links to Kotlin declarations, shown as code (outside the code spans)
                .split('`')
                .mapIndexed { index, part -> if (index % 2 == 0) part.replace(link) { "`${it.groupValues[1]}`" } else part }
                .joinToString("`")
        }
}

/** An area of the modules, as ModuleRegistry groups them */
class Area(
    val title: String,
    val description: String,
    val modules: List<ArgLibrary>,
) {
    /** The directory of its pages */
    val slug: String = title.lowercase().replace(' ', '-')
}

/** The pages of the reference */
class PrologReference(
    kdocs: KDocs,
) {
    private val areas =
        listOf(
            Area("Entry points", "The predicates a theory is queried with, and what they rely on.", ModuleRegistry.support),
            Area("Data", "The terms arg2p works on: the entities of argumentation graphs and of the rules.", ModuleRegistry.data),
            Area("Language", "The rules of a theory, as the graph builders read them.", ModuleRegistry.language),
            Area("Graph", "Building the argumentation graph: its arguments and attacks, and where they are kept.", ModuleRegistry.graph),
            Area("Graph extensions", "Changing the argumentation graph: preferences, restrictions, burdens.", ModuleRegistry.extensions),
            Area(
                "Evaluation",
                "Evaluating the graph: abstract frameworks, the whole graph of a theory, or the part a query depends on.",
                ModuleRegistry.evaluation,
            ),
            Area("Labelling", "The semantics labelling the arguments, and the statements from them.", ModuleRegistry.labellings),
        )

    val modules: List<ModuleDoc> =
        areas.flatMap { area ->
            area.modules.filterIsInstance<Loadable>().map { module ->
                ModuleDoc(module as ArgLibrary, area, kdocs)
            }
        }

    private val byIdentifier = modules.associateBy { it.identifier }

    // The predicates the engine of arg2p calls in the modules a flag selects
    private val dispatched =
        setOf(
            Signature("buildArgumentationGraph", 0),
            Signature("buildAdditionalArguments", 0),
            Signature("buildAttacks", 0),
            Signature("modifyArgumentationGraph", 0),
            Signature("argumentLabelling", 0),
            Signature("statementLabelling", 0),
            Signature("locality", 1),
            Signature("queryDependencies", 2),
            Signature("queryDependants", 2),
        )

    private fun callers(module: ModuleDoc) = modules.filter { module.identifier in it.calls }

    // The predicates of the module other modules call, or the engine dispatches to, with no documentation
    private fun undocumented(module: ModuleDoc): List<Signature> {
        val called = modules.flatMap { it.calls[module.identifier].orEmpty() }.toSet()
        val documented = module.entries.map { it.indicator }.toSet()
        return module.defined.filter { (it in called || it in dispatched) && it !in documented }
    }

    private fun summary(text: String) =
        text
            .substringBefore("\n\n")
            .replace("\n", " ")
            .substringBefore(". ")
            .removeSuffix(".")

    private fun Signature.indicator() = "$name/$arity"

    private fun Signature.anchor() = "$name-$arity"

    private fun link(identifier: String) =
        byIdentifier[identifier]?.let { "[$identifier]({{% ref \"/docs/api/prolog/${it.area.slug}/$identifier\" %}})" } ?: "`$identifier`"

    /** The pages, by their path: the index, the section of each area and the page of each module */
    fun pages(): Map<String, String> =
        mapOf("_index.md" to index()) +
            areas.mapIndexed { index, area -> "${area.slug}/_index.md" to section(area, index + 1) } +
            modules.map { "${it.area.slug}/${it.identifier}.md" to page(it) }

    // The modules of an area, with the number of their predicates and what they are
    private fun table(area: Area) =
        buildString {
            appendLine("| Module | Predicates | Description |")
            appendLine("| --- | ---: | --- |")
            modules.filter { it.area === area }.forEach { module ->
                appendLine("| ${link(module.identifier)} | ${module.entries.size} | ${summary(module.description)} |")
            }
        }

    private fun section(
        area: Area,
        weight: Int,
    ): String =
        buildString {
            appendLine("---")
            appendLine("title: ${area.title}")
            appendLine("weight: $weight")
            appendLine("bookCollapseSection: true")
            appendLine("---")
            appendLine()
            appendLine("<!-- Generated from the sources by prologDocs (core) -->")
            appendLine()
            appendLine("## ${area.title}")
            appendLine()
            appendLine(area.description)
            appendLine()
            append(table(area))
        }

    fun index(): String =
        buildString {
            appendLine("---")
            appendLine("title: Prolog modules")
            appendLine("weight: 40")
            appendLine("bookCollapseSection: true")
            appendLine("---")
            appendLine()
            appendLine("<!-- Generated from the sources by prologDocs (core): edit the modules, not this page -->")
            appendLine()
            appendLine("## Prolog modules")
            appendLine()
            appendLine("Every predicate of the modules of arg2p, from their sources. A module is called as `module::Goal`, or")
            appendLine("`module:::Goal` to run the goal in a new context branch (see [Modules]({{% ref \"/docs/modules\" %}})).")
            appendLine()
            appendLine("In the signatures, `+Arg` is an input, bound when called, `-Arg` an output, `?Arg` either of the two.")
            areas.forEach { area ->
                appendLine()
                appendLine("### ${area.title}")
                appendLine()
                appendLine(area.description)
                appendLine()
                append(table(area))
            }
        }

    fun page(module: ModuleDoc): String =
        buildString {
            appendLine("---")
            appendLine("title: ${module.identifier}")
            appendLine("weight: ${modules.indexOf(module) + 1}")
            appendLine("---")
            appendLine()
            appendLine("<!-- Generated from the sources by prologDocs (core): edit ${module.objectName}, not this page -->")
            appendLine()
            appendLine("## ${module.identifier}")
            appendLine()
            appendLine("${module.area.title} · Kotlin object `${module.objectName}` · library `${module.module.alias}`")
            appendLine()
            if (module.description.isNotBlank()) {
                appendLine(module.description)
                appendLine()
            }
            val uses = module.calls.keys.sorted()
            if (uses.isNotEmpty()) appendLine("- **Calls:** " + uses.joinToString(", ") { link(it) })
            val callers = callers(module).map { it.identifier }
            if (callers.isNotEmpty()) appendLine("- **Called by:** " + callers.joinToString(", ") { link(it) })
            val flags = module.module.flags().map { it.predicate() }
            if (flags.isNotEmpty()) {
                appendLine("- **Flags:** " + flags.joinToString(", ") { "`$it`" } + " (see [Flags]({{% ref \"/docs/api/flags\" %}}))")
            }
            if (module.entries.isNotEmpty()) {
                appendLine()
                appendLine("| Predicate | Description |")
                appendLine("| --- | --- |")
                module.entries.forEach {
                    appendLine("| [`${it.indicator.indicator()}`](#${it.indicator.anchor()}) | ${summary(it.description)} |")
                }
                module.entries.forEach { predicate ->
                    appendLine()
                    appendLine("### ${predicate.indicator.indicator()} {#${predicate.indicator.anchor()}}")
                    appendLine()
                    appendLine("```prolog")
                    appendLine(predicate.signature)
                    appendLine("```")
                    appendLine()
                    if (predicate.indicator in module.primitives) {
                        appendLine("_Native primitive._")
                        appendLine()
                    }
                    appendLine(predicate.description)
                }
            }
            val undocumented = undocumented(module)
            if (undocumented.isNotEmpty()) {
                appendLine()
                appendLine("### Undocumented")
                appendLine()
                appendLine("Called by other modules, or dispatched to by the engine, with no `%!` comment:")
                appendLine()
                undocumented.forEach { appendLine("- `${it.indicator()}`") }
            }
        }
}
