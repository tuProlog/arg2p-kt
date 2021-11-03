package it.unibo.tuprolog.argumentation.core.libs.utils

actual object Utils : UtilsBase() {
    override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.Utils.theoryCode
}
