package it.unibo.tuprolog.argumentation.core.flags

import it.unibo.tuprolog.argumentation.core.runtime.settings.Profiles

/**
 * The framework a theory is read in, its profile (see [Profiles]): a shortcut for the values of the flags the profile
 * presets. ASPIC+ by default; in the profile free no flag is preset.
 */
object GraphBuildMode : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "graphBuildMode"

    override fun default(): String = Profiles.default.name

    override fun values(): Iterable<String> = Profiles.all.map { it.name }
}
