package it.unibo.tuprolog.argumentation.core.flags

import it.unibo.tuprolog.argumentation.core.runtime.settings.Profiles

/**
 * The framework a theory is read in, its profile (see [Profiles]): a shortcut for the values of the flags the profile
 * presets. In the profile free (the default) no flag is preset.
 */
object GraphBuildMode : ArgsFlag<String, Iterable<String>> {
    override fun predicate(): String = "graphBuildMode"

    override fun default(): String = Profiles.FREE.name

    override fun values(): Iterable<String> = Profiles.all.map { it.name }
}
