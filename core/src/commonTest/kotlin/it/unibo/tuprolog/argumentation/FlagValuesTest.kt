package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag
import kotlin.test.Test
import kotlin.test.assertEquals
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.pipeline.flags.ArgumentLabellingMode as ArgumentLabellingModeFlag
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.pipeline.flags.GraphBuilder as GraphBuilderFlag
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.pipeline.flags.GraphExtension as GraphExtensionFlag
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.pipeline.flags.StatementLabellingMode as StatementLabellingModeFlag
import it.unibo.tuprolog.argumentation.core.libraries.framework.parser.flags.GraphLanguage as GraphLanguageFlag
import it.unibo.tuprolog.argumentation.core.libraries.framework.settings.flags.GraphBuildMode as GraphBuildModeFlag
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.preference.superiority.flags.OrderingComparator as OrderingComparatorFlag
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.preference.superiority.flags.OrderingPrinciple as OrderingPrincipleFlag
import it.unibo.tuprolog.argumentation.core.libraries.language.conflict.flags.ConflictModel as ConflictModelFlag

// The values of the enumerations of the API are those the flags of arg2p accept, each of them
class FlagValuesTest {
    private fun assertSameValues(
        flag: ArgsFlag,
        values: List<String>,
    ) = assertEquals(flag.values().toSet(), values.toSet(), flag.predicate())

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
