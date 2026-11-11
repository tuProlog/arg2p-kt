package it.unibo.tuprolog.argumentation.core.libraries.framework.settings.flags

import it.unibo.tuprolog.argumentation.Profile
import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag

/**
 * The framework a theory is read in, its profile (see the object Profiles): a shortcut for the values of the flags the profile
 * presets. ASPIC+ by default; in the profile free no flag is preset.
 */
object GraphBuildMode : ArgsFlag {
    override fun predicate(): String = "graphBuildMode"

    override fun default(): String = Profile.ASPIC_PLUS.value

    override fun values(): Iterable<String> = Profile.entries.map { it.value }
}
