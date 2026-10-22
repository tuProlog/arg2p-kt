package it.unibo.tuprolog.argumentation.core.runtime.settings

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Truth
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.DomainError
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.theory.Theory

/**
 * The flags of an arg2p solver: those its libraries declare ([ArgsFlag]), checked by the Prolog code through
 * setting/1 (whether a switch is on) and setting/2 (the values of a flag); checking a flag no library declares, or a
 * switch for its values (and the other way round), raises a domain error.
 *
 * Their values are a view over the facts of the libraries (the settings of the solver, as [FlagsBuilder] states them)
 * and those of the theory: for each flag, the theory overrides the libraries.
 */
class Settings(
    private val solver: Arg2pSolver,
) : ArgLibrary {
    private val flags: Map<String, ArgsFlag<*, *>> by lazy {
        (solver.baseLibraries() + solver.staticLibraries() + solver.dynamicLibraries())
            .flatMap { it.flags() }
            .associateBy { it.predicate() }
    }

    // The values of the flags, for the last theory met (the settings of the libraries are those of the solver)
    private var viewOf: Theory? = null
    private var view: Map<String, List<Term>> = emptyMap()

    /** Whether the switch [flag] is on in [context] */
    fun isOn(
        flag: ArgsFlag<*, *>,
        context: ExecutionContext,
    ): Boolean = valuesOf(flag, context).isNotEmpty()

    /** The values of [flag] in [context] (for a switch, `true` if it is on) */
    fun valuesOf(
        flag: ArgsFlag<*, *>,
        context: ExecutionContext,
    ): List<Term> = viewFor(context)[flag.predicate()].orEmpty()

    private fun viewFor(context: ExecutionContext): Map<String, List<Term>> {
        if (viewOf !== context.staticKb) {
            val libraries = context.libraries.asTheory(context.unificator)
            view = flags.mapValues { (_, flag) -> stated(flag, context.staticKb).ifEmpty { stated(flag, libraries) } }
            viewOf = context.staticKb
        }
        return view
    }

    // The values of a flag stated by the facts of a theory
    private fun stated(
        flag: ArgsFlag<*, *>,
        theory: Theory,
    ): List<Term> {
        val pattern = if (flag.isSwitch) Atom.of(flag.predicate()) else Struct.of(flag.predicate(), Var.of("Value"))
        return theory[pattern]
            .filter { it.isFact }
            .map { if (flag.isSwitch) Truth.TRUE else it.head[0] }
            .toList()
    }

    // The flag named by the first argument of the request, declared of the kind its check expects
    private fun declared(
        request: Solve.Request<ExecutionContext>,
        switch: Boolean,
    ): ArgsFlag<*, *> {
        val name = request.arguments[0]
        if (name !is Atom) throw TypeError.forArgument(request.context, request.signature, TypeError.Expected.ATOM, name, 0)
        return flags[name.value]?.takeIf { it.isSwitch == switch }
            ?: throw DomainError.forArgument(request.context, request.signature, DomainError.Expected.FLAG, name, 0)
    }

    /** setting(+Name): the switch Name is on */
    inner class Switch : PrimitiveWithSignature {
        override val signature = SWITCH

        // The settings this primitive checks, for the native primitives of the other libraries
        val owner: Settings
            get() = this@Settings

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
            sequenceOf(request.replyWith(isOn(declared(request, switch = true), request.context)))
    }

    /** setting(+Name, ?Value): Value is a value of the flag Name, in the order they are stated */
    inner class Value : PrimitiveWithSignature {
        override val signature = Signature("setting", 2)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val values = valuesOf(declared(request, switch = false), request.context)
            return values.asSequence().mapNotNull { value ->
                Arg2pUnificator
                    .mgu(request.arguments[1], value)
                    .takeIf { it.isSuccess }
                    ?.let { request.replyWith(it) }
            }
        }
    }

    override val alias = "prolog.argumentation.settings"

    override val baseContent: Library
        get() =
            listOf(Switch(), Value()).let { primitives ->
                Library.of(
                    alias = this.alias,
                    primitives = primitives.associateBy { it.signature },
                )
            }

    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    companion object {
        private val SWITCH = Signature("setting", 1)

        /** The settings of the solver of [context], if they are reachable */
        fun of(context: ExecutionContext): Settings? = (context.libraries.primitives[SWITCH] as? Switch)?.owner
    }
}
