package it.unibo.tuprolog.argumentation.core.primitives.extra

import it.unibo.tuprolog.argumentation.SyntaxProblem
import it.unibo.tuprolog.argumentation.core.engine.AbstractPrologSolver
import it.unibo.tuprolog.argumentation.core.flags.ModulesPath
import it.unibo.tuprolog.argumentation.core.model.terms.TermValues
import it.unibo.tuprolog.argumentation.core.runtime.modules.solveForCaller
import it.unibo.tuprolog.argumentation.core.runtime.settings.Settings
import it.unibo.tuprolog.argumentation.core.runtime.theory.RuleAnnotations
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
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
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.theory.Theory

/**
 * call_module(+Modules, +Goal): Goal solved on the theories of Modules alone, loaded in their order. Each module is an
 * atom: a location, a URL or the path of a file ending in `.pl` (relative to the working directory), or else the name
 * of a file in the modules path (flag `modulesPath`, the working directory if there is none), `.pl` left out. A module
 * that cannot be read raises an existence error, one that does not parse a syntax error; an error raised within the
 * modules is raised by the call.
 */
internal object CallModule : Primitive {
    val signature = Signature("call_module", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val modules: Term = request.arguments[0]
        val goal: Term = request.arguments[1]

        if (modules !is List) {
            throw TypeError.forArgument(request.context, request.signature, TypeError.Expected.LIST, modules, 0)
        }
        if (goal !is Struct) {
            throw TypeError.forArgument(request.context, request.signature, TypeError.Expected.CALLABLE, goal, 1)
        }
        val names =
            modules.toList().map {
                it as? Atom ?: throw TypeError.forArgument(request.context, request.signature, TypeError.Expected.ATOM, it, 0)
            }

        val base = modulesPath(request.context)
        val solver = solverOf(request.context, names.map { locationOf(it.value, base) })
        return solver.solveForCaller(goal, SolveOptions.allLazily()).map {
            when (it) {
                is Solution.Yes -> request.replySuccess(it.substitution)
                // an error inside the modules surfaces: a failure would turn a broken call into an empty result
                is Solution.Halt -> throw it.exception
                else -> request.replyFail()
            }
        }
    }

    // The modules path of the settings, if there is one
    private fun modulesPath(context: ExecutionContext): String? =
        Settings
            .of(context)
            ?.valuesOf(ModulesPath, context)
            ?.firstOrNull()
            ?.let { (it as? Atom)?.value ?: it.toString() }

    // The location of the module [name]: a URL, or a path ending in .pl, as it is; else the file of the module in [base]
    private fun locationOf(
        name: String,
        base: String?,
    ): String =
        when {
            isUrl(name) || name.endsWith(".pl") -> name
            base == null -> "$name.pl"
            else -> "${base.trimEnd('/', '\\')}/$name.pl"
        }

    // A location carrying a scheme (http://, file://): the others are paths of files
    private fun isUrl(location: String) = location.contains("://")

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

    // The theory of the module at [location], read with the operators of every theory of arg2p (those of the module
    // calls, ::, among them): a syntax error if it does not parse
    private fun theoryOf(
        context: ExecutionContext,
        location: String,
    ): Theory {
        val text = read(context, location)
        return try {
            RuleAnnotations.parse(text, TermValues.operators)
        } catch (e: ParseException) {
            throw SyntaxError.of(context, "$location: ${SyntaxProblem.of(e)}")
        }
    }

    // A solver of the modules at [locations], each adding its clauses to those before it, with the libraries and the
    // flags of the calling context (the theories of arg2p rely on unknown predicates failing, which the default flags
    // do not give), and IOLib, for the modules using its predicates: the theory of the caller is left out
    private fun solverOf(
        context: ExecutionContext,
        locations: kotlin.collections.List<String>,
    ): Solver =
        context
            .createMutableSolver(libraries = context.libraries + IOLib, staticKb = Theory.empty())
            .also { solver -> locations.forEach { solver.appendStaticKb(theoryOf(context, it)) } }
            // the theory of the solver is the one of the modules, which its goals see wherever call_module/2 is called
            .also { solver -> (solver as? AbstractPrologSolver)?.theoryVisible = true }
}
