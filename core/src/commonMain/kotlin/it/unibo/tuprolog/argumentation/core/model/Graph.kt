package it.unibo.tuprolog.argumentation.core.model

data class Graph(
    val arguments: List<Argument>,
    val attacks: List<Attack>,
    val supports: List<Support>,
) {
    var labellings: List<LabelledArgument> = emptyList()

    companion object {
        fun of(
            labellings: List<LabelledArgument>,
            attacks: List<Attack>,
            supports: List<Support>,
        ) = Graph(labellings.map { it.argument }, attacks, supports).also {
            it.labellings = labellings
        }
    }
}
