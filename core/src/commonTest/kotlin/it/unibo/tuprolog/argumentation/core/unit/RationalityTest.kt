package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.TestingUtils
import it.unibo.tuprolog.argumentation.core.TestingUtils.buildLabelSets
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

class RationalityTest {

    private val example4theory =
        """
        f1 :-> wr.
        f2 :-> go.
        s1: b -> -hw.
        s2: m -> hw.
        d1: wr => m.
        d2: go => b.
        graphBuildMode(base).
        statementLabellingMode(base).
        argumentLabellingMode(grounded).
        orderingPrinciple(last).
        orderingComparator(elitist).
        
        """.trimIndent()

    private val example5theory =
        """
        f1 :-> a.
        f2 :-> d.
        f3 :-> c.
        s1 : b,e -> -c.
        d1 : a => b.
        d2 : d => e.
        graphBuildMode(base).
        statementLabellingMode(base).
        argumentLabellingMode(grounded).
        orderingPrinciple(last).
        orderingComparator(elitist).
        
        """.trimIndent()

    private val example6theory =
        """
        f1 :-> a.
        f2 :-> d.
        f3 :-> g.
        s1 : b,c,e,f -> -g.
        d1 : a => b.
        d2 : b => c.
        d3 : d => e.
        d4 : e => f.
        graphBuildMode(base).
        statementLabellingMode(base).
        argumentLabellingMode(grounded).
        orderingPrinciple(last).
        orderingComparator(elitist).
        
        """.trimIndent()

    private val example7theory =
        """
        f1 :-> a.
        f2 :-> b.
        f3 :-> c.
        f4 :-> g.
        s1 : d,e,f -> -g.
        d1 : a => d.
        d2 : b => e.
        d3 : c => f.
        graphBuildMode(base).
        statementLabellingMode(base).
        orderingPrinciple(last).
        orderingComparator(elitist). 
        
        """.trimIndent()

    @Test
    fun caminadaExample4() = buildLabelSets(
        example4theory + "unrestrictedRebut.",
        "[[wr],[m],[go],[b]]",
        "[]",
        "[[neg, hw],[hw]]"
    ).let { }

    @Test
    fun caminadaExample4restricted() = buildLabelSets(
        example4theory,
        "[[wr],[neg, hw],[m],[hw],[go],[b]]",
        "[]",
        "[]"
    ).let { }

    @Test
    fun caminadaExample4transposed() = buildLabelSets(
        example4theory + """
            autoTransposition.
            unrestrictedRebut.
        """.trimIndent(),
        "[[wr],[go]]",
        "[]",
        "[[neg, m],[neg, hw],[neg, b],[m],[hw],[b]]"
    ).let { }

    @Test
    fun caminadaExample5() = buildLabelSets(
        example5theory + "unrestrictedRebut.",
        "[[e],[d],[c],[b],[a]]",
        "[[neg,c]]",
        "[]"
    ).let { }

    @Test
    fun caminadaExample5restricted() = buildLabelSets(
        example5theory,
        "[[neg,c],[e],[d],[c],[b],[a]]",
        "[]",
        "[]"
    ).let { }

    @Test
    fun caminadaExample5transposed() = buildLabelSets(
        example5theory + """
            autoTransposition.
            unrestrictedRebut.
        """.trimIndent(),
        "[[d],[c],[a]]",
        "[[neg,c]]",
        "[[neg,e],[neg,b],[e],[b]]"
    ).let { }

    @Test
    fun caminadaExample6() = buildLabelSets(
        example6theory + "unrestrictedRebut.",
        "[[g],[f],[e],[d],[c],[b],[a]]",
        "[[neg,g]]",
        "[]"
    ).let { }

    @Test
    fun caminadaExample6restricted() = buildLabelSets(
        example6theory,
        "[[neg,g],[g],[f],[e],[d],[c],[b],[a]]",
        "[]",
        "[]"
    ).let { }

    @Test
    fun caminadaExample6transposed() = buildLabelSets(
        example6theory + """
            autoTransposition.
            unrestrictedRebut.
        """.trimIndent(),
        "[[g],[d],[a]]",
        "[[neg,g]]",
        "[[neg,f],[neg,e],[neg,c],[neg,b],[f],[e],[c],[b]]"
    ).let { }

    @Test
    fun caminadaExample7transposed() = buildLabelSets(
        example7theory + """
            argumentLabellingMode(grounded).
            autoTransposition.
            unrestrictedRebut.
        """.trimIndent(),
        "[[g],[c],[b],[a]]",
        "[[neg,g]]",
        "[[neg,f],[neg,e],[neg,d],[f],[e],[d]]"
    ).let { }

    @Test
    fun caminadaExample7completeSemantic() {
        assertEquals(
            11,
            TestingUtils.solverWithTheory(
                example7theory + """
                argumentLabellingMode(complete).
                autoTransposition.
                unrestrictedRebut.
                """.trimIndent()
            ).solve(Struct.parse("buildLabelSets")).filter { it.isYes }.count()
        )

        assertEquals(
            4,
            TestingUtils.solverWithTheory(
                example7theory + """
                argumentLabellingMode(complete).
                autoTransposition.
                """.trimIndent()
            ).solve(Struct.parse("buildLabelSets")).filter { it.isYes }.count()
        )
    }
}
