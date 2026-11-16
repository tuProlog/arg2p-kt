package it.unibo.tuprolog.argumentation.core.libraries.language.moduleCalls.primitives

import it.unibo.tuprolog.argumentation.SyntaxProblem
import it.unibo.tuprolog.argumentation.TermValues
import it.unibo.tuprolog.argumentation.core.libraries.framework.parser.helpers.RuleAnnotations
import it.unibo.tuprolog.argumentation.core.libraries.framework.settings.Settings
import it.unibo.tuprolog.argumentation.core.libraries.language.moduleCalls.flags.ModulesPath
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.answersOf
import it.unibo.tuprolog.argumentation.engine.natives.termArg
import it.unibo.tuprolog.argumentation.engine.solver.VmSolver
import it.unibo.tuprolog.argumentation.engine.term.Arg2pUnificator
import it.unibo.tuprolog.argumentation.engine.term.haltsRaised
import it.unibo.tuprolog.argumentation.engine.term.solveForCaller
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.exception.TuPrologException
import it.unibo.tuprolog.core.parsing.ParseException
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.SolveOptions
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.solve.exception.error.ExistenceError
import it.unibo.tuprolog.solve.exception.error.SyntaxError
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.libs.io.IOLib
import it.unibo.tuprolog.solve.libs.io.Url
import it.unibo.tuprolog.theory.Theory

/**
 * callModule(+Modules, +Goal): Goal solved on the theories of Modules alone, their texts read as one in their order (the
 * operators a module declares hold in those after it). Each module is an
 * atom: a URL or an absolute path, as it is, or else a path relative to the modules path (flag `modulesPath`, a
 * directory or a base URL; the working directory if there is none), `.pl` added if it is left out. A module
 * that cannot be read raises an existence error, one that does not parse a syntax error; an error raised within the
 * modules is raised by the call.
 */
internal object CallModule : AbstractNativePrimitive(Signature("callModule", 2)) {
    override fun call(call: NativeCall): Int {
        val context = call.context
        val modules = call.termArg(0)
        val goal = call.termArg(1)

        if (modules !is List) {
            throw TypeError.forArgument(context, signature, TypeError.Expected.LIST, modules, 0)
        }
        if (goal !is Struct) {
            throw TypeError.forArgument(context, signature, TypeError.Expected.CALLABLE, goal, 1)
        }
        val names =
            modules.toList().map {
                it as? Atom ?: throw TypeError.forArgument(context, signature, TypeError.Expected.ATOM, it, 0)
            }

        val base = modulesPath(context)
        val solver = solverOf(context, names.map { locationOf(it.value, base) })
        val solutions = solver.solveForCaller(goal, SolveOptions.allLazily()).haltsRaised().filterIsInstance<Solution.Yes>()
        return call.answersOf(solutions.map { it.substitution }.iterator())
    }

    // The modules path of the settings, if there is one
    private fun modulesPath(context: ExecutionContext): String? =
        Settings
            .of(context)
            ?.valuesOf(ModulesPath, context)
            ?.firstOrNull()
            ?.let { (it as? Atom)?.value ?: it.toString() }

    // The location of the module [name]: a URL or an absolute path, as it is; else the path relative to [base] (the
    // working directory if there is none), .pl added if it is left out
    private fun locationOf(
        name: String,
        base: String?,
    ): String {
        if (isUrl(name) || isAbsolute(name)) return name
        val file = if (name.endsWith(".pl")) name else "$name.pl"
        return if (base == null) file else "${base.trimEnd('/', '\\')}/$file"
    }

    // A location carrying a scheme (http://, file://): the others are paths of files
    private fun isUrl(location: String) = location.contains("://")

    // A path from the root (/x, \x) or from a drive (C:\x, C:/x)
    private fun isAbsolute(location: String) =
        location.startsWith("/") ||
            location.startsWith("\\") ||
            (location.length > 2 && location[0].isLetter() && location[1] == ':' && location[2] in "/\\")

    // The text of the module at [location]: an existence error if it cannot be read
    private fun read(
        context: ExecutionContext,
        location: String,
    ): String =
        try {
            (if (isUrl(location)) Url.of(location) else Url.file(location)).readAsText()
        } catch (e: TuPrologException) {
            throw ExistenceError.of(
                context,
                ExistenceError.ObjectType.SOURCE_SINK,
                Atom.of(location),
                "No module can be read at `$location`: ${e.message}",
            )
        }

    // The theory of the modules at [locations], their texts read as one, in their order (an operator a module declares
    // holds in those after it), with the operators of every theory of arg2p (those of the module calls, ::, among
    // them): a syntax error if it does not parse, reported at the module and the line of that module where it is
    private fun theoryOf(
        context: ExecutionContext,
        locations: kotlin.collections.List<String>,
    ): Theory {
        val texts = locations.map { read(context, it).let { text -> if (text.endsWith("\n")) text else text + "\n" } }
        return try {
            RuleAnnotations.parse(texts.joinToString(""), TermValues.operators, Arg2pUnificator)
        } catch (e: ParseException) {
            val problem = SyntaxProblem.of(e)
            // the module the line is in, and the line within it
            var first = 1
            val index =
                texts.indices.firstOrNull { i ->
                    val lines = texts[i].count { it == '\n' }
                    (problem.line < first + lines).also { inside -> if (!inside) first += lines }
                } ?: texts.lastIndex
            throw SyntaxError.of(
                context,
                "${locations[index]}: ${problem.description} at ${problem.line - first + 1}:${problem.column}",
            )
        }
    }

    // A solver of the modules at [locations] (see theoryOf), with the libraries and the flags of the calling context
    // (the theories of arg2p rely on unknown predicates failing, which the default flags do not give), and IOLib, for
    // the modules using its predicates: the theory of the caller is left out
    private fun solverOf(
        context: ExecutionContext,
        locations: kotlin.collections.List<String>,
    ): Solver =
        context
            .createMutableSolver(libraries = context.libraries + IOLib, staticKb = Theory.empty())
            .also { solver -> solver.appendStaticKb(theoryOf(context, locations)) }
            // the theory of the solver is the one of the modules, which its goals see wherever callModule/2 is called
            .also { solver -> (solver as? VmSolver)?.theoryVisible = true }
}
