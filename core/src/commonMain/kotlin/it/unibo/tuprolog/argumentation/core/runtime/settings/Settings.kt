package it.unibo.tuprolog.argumentation.core.runtime.settings

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.argumentation.core.flags.GraphBuildMode
import it.unibo.tuprolog.argumentation.core.modules.ArgLibrary
import it.unibo.tuprolog.argumentation.core.primitives.settings.SettingSwitch
import it.unibo.tuprolog.argumentation.core.primitives.settings.SettingValue
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Fact
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Truth
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.ExecutionContext
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
 * and those of the theory, through the profile of the theory (graphBuildMode, see [Profiles]): for each flag, the
 * theory states its values, or else the settings of the solver if they differ from the default of the flag, or else
 * the profile, or else the settings of the solver. A flag is stated with no value by `Flag(none)` (no graph
 * extension, say), a switch stated off by `Switch(off)`: so a theory clears what its profile presets. A value
 * standing for others ([ArgsFlag.aliases], the builtin of conflictModel) states those, and the values tell them.
 */
class Settings(
    private val solver: Arg2pSolver,
) : ArgLibrary {
    /** The flags the libraries of the solver declare, by name */
    val flags: Map<String, ArgsFlag<*, *>> by lazy {
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

    /**
     * The facts stating the flags of [context] with their values (see [valuesOf]), but for the flags named in
     * [except]: for a theory solved elsewhere with the same flags
     */
    fun facts(
        context: ExecutionContext,
        except: Set<String> = emptySet(),
    ): List<Fact> =
        flags.filterKeys { it !in except }.flatMap { (name, flag) ->
            valuesOf(flag, context).map { value -> Fact.of(if (flag.isSwitch) Atom.of(name) else Struct.of(name, value)) }
        }

    /** Whether [clause] is a fact stating a flag */
    fun states(clause: Clause): Boolean {
        val head = clause.head ?: return false
        val flag = flags[head.functor] ?: return false
        return clause.isFact && (head.arity == 1 || (flag.isSwitch && head.arity == 0))
    }

    private fun viewFor(context: ExecutionContext): Map<String, List<Term>> {
        if (viewOf !== context.staticKb) {
            val libraries = context.libraries.asTheory(context.unificator)
            val theory = flags.mapValues { (_, flag) -> stated(flag, context.staticKb) }
            val solver = flags.mapValues { (_, flag) -> stated(flag, libraries) }
            val mode = theory[GraphBuildMode.predicate()].orEmpty().ifEmpty { solver[GraphBuildMode.predicate()].orEmpty() }
            val profile = if (mode.isEmpty()) Profiles.default else Profiles.named((mode.first() as? Atom)?.value)
            val presets = profile?.presets.orEmpty()
            view =
                flags.mapValues { (name, flag) ->
                    // a switch of the solver is off with no fact, another flag has no value stated
                    val stated = solver.getValue(name)
                    // the values of the default in any order (those of a set of parts, as conflictModel)
                    val explicit = (flag.isSwitch || stated.isNotEmpty()) && stated.map { it.toString() }.toSet() != defaultOf(flag).toSet()
                    val preset = presets[name]?.let { expanded(flag, it) }
                    val values =
                        when {
                            theory.getValue(name).isNotEmpty() -> theory.getValue(name)
                            explicit -> stated
                            flag.isSwitch -> preset ?: stated
                            else -> preset ?: stated.ifEmpty { defaultOf(flag).map { Atom.of(it) } }
                        }
                    values.filter { it != NONE && it != OFF }
                }
            viewOf = context.staticKb
        }
        return view
    }

    // The values of a flag stated by the facts of a theory: none or off for one stated with no value
    private fun stated(
        flag: ArgsFlag<*, *>,
        theory: Theory,
    ): List<Term> {
        val valued = theory[Struct.of(flag.predicate(), Var.of("Value"))].filter { it.isFact }.map { it.head[0] }
        if (!flag.isSwitch) return expanded(flag, valued.toList())
        return theory[Atom.of(flag.predicate())].filter { it.isFact }.map { Truth.TRUE }.toList() + valued.filter { it == OFF }
    }

    // The values of a flag, those standing for others replaced by them (see ArgsFlag.aliases), each once
    private fun expanded(
        flag: ArgsFlag<*, *>,
        values: List<Term>,
    ): List<Term> =
        if (flag.aliases.isEmpty()) {
            values
        } else {
            values.flatMap { value -> flag.aliases[(value as? Atom)?.value]?.map { Atom.of(it) } ?: listOf(value) }.distinct()
        }

    // The default values of a flag, as text
    private fun defaultOf(flag: ArgsFlag<*, *>): List<String> =
        when (val default = flag.default()) {
            is Boolean -> if (default) listOf(Truth.TRUE.toString()) else emptyList()
            is Iterable<*> -> default.map { Atom.of(it.toString()).toString() }
            else -> listOf(Atom.of(default.toString()).toString())
        }

    /** The flag named by the first argument of [request], declared of the kind its check expects (a switch or not) */
    internal fun declared(
        request: Solve.Request<ExecutionContext>,
        switch: Boolean,
    ): ArgsFlag<*, *> {
        val name = request.arguments[0]
        if (name !is Atom) throw TypeError.forArgument(request.context, request.signature, TypeError.Expected.ATOM, name, 0)
        return flags[name.value]?.takeIf { it.isSwitch == switch }
            ?: throw DomainError.forArgument(request.context, request.signature, DomainError.Expected.FLAG, name, 0)
    }

    override val alias = "prolog.argumentation.settings"

    override val baseContent: Library
        get() =
            listOf(SettingSwitch(this), SettingValue(this)).let { primitives ->
                Library.of(
                    alias = this.alias,
                    primitives = primitives.associateBy { it.signature },
                )
            }

    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    companion object {
        // The values stating a flag with no value, and a switch off
        private val NONE = Atom.of("none")
        private val OFF = Atom.of("off")

        /** The settings of the solver of [context], if they are reachable */
        fun of(context: ExecutionContext): Settings? = (context.libraries.primitives[SettingSwitch.SIGNATURE] as? SettingSwitch)?.owner
    }
}
