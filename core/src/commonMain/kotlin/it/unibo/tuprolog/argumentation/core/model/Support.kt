package it.unibo.tuprolog.argumentation.core.model

data class Support(val supporter: Argument, val supported: Argument) {
    override fun toString(): String {
        return "support($supporter, $supported)"
    }
}
