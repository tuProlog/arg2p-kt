package it.unibo.tuprolog.argumentation.core.model

data class LabelledArgument(val argument: Argument, val label: Label) {
    override fun toString(): String {
        return "$label($argument)"
    }
}
