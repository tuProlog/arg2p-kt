package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import kotlin.test.Test
import kotlin.test.assertEquals
import it.unibo.tuprolog.argumentation.core.flags.ArgumentLabellingMode as ArgumentLabellingModeFlag
import it.unibo.tuprolog.argumentation.core.flags.ConflictModel as ConflictModelFlag
import it.unibo.tuprolog.argumentation.core.flags.GraphBuildMode as GraphBuildModeFlag
import it.unibo.tuprolog.argumentation.core.flags.GraphBuilder as GraphBuilderFlag
import it.unibo.tuprolog.argumentation.core.flags.GraphExtension as GraphExtensionFlag
import it.unibo.tuprolog.argumentation.core.flags.GraphLanguage as GraphLanguageFlag
import it.unibo.tuprolog.argumentation.core.flags.OrderingComparator as OrderingComparatorFlag
import it.unibo.tuprolog.argumentation.core.flags.OrderingPrinciple as OrderingPrincipleFlag
import it.unibo.tuprolog.argumentation.core.flags.StatementLabellingMode as StatementLabellingModeFlag

// The values of the enumerations of the API are those the flags of arg2p accept, each of them
class FlagValuesTest {
    private fun assertSameValues(
        flag: ArgsFlag<*, *>,
        values: List<String>,
    ) = assertEquals((flag.values() as Iterable<*>).map { it.toString() }.toSet(), values.toSet(), flag.predicate())

    @Test
    fun theValuesOfEachFlag() {
        assertSameValues(GraphBuildModeFlag, Profile.entries.map { it.value })
        assertSameValues(ArgumentLabellingModeFlag, Semantics.entries.map { it.value })
        assertSameValues(StatementLabellingModeFlag, StatementLabelling.entries.map { it.value })
        assertSameValues(OrderingPrincipleFlag, OrderingPrinciple.entries.map { it.value })
        assertSameValues(OrderingComparatorFlag, OrderingComparator.entries.map { it.value })
        assertSameValues(GraphExtensionFlag, GraphExtension.entries.map { it.value })
        assertSameValues(GraphLanguageFlag, GraphLanguage.entries.map { it.value })
        assertSameValues(GraphBuilderFlag, GraphBuilder.entries.map { it.value })
        assertSameValues(ConflictModelFlag, ConflictModel.entries.map { it.value })
    }
}
