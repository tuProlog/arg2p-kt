package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.TestingUtils
import it.unibo.tuprolog.argumentation.core.TestingUtils.answerQuery
import it.unibo.tuprolog.argumentation.core.TestingUtils.buildLabelSets
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Ignore
import kotlin.test.Test
import kotlin.test.assertEquals

@Ignore
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

    private fun protoTest(theory: String, argsIn: Iterable<String>, argsOut: Iterable<String>, argsUnd: Iterable<String>) {
        val prepare = { term: String -> if (term.startsWith("-")) "[neg,${term.removePrefix("-")}]" else "[$term]" }
        val parse = { args: Iterable<String> -> args.map { prepare(it) }.joinToString(",", "[", "]") }

        argsIn.forEach { answerQuery("$theory\nqueryMode.", it, "[$it]", "[]", "[]") }
        argsOut.forEach { answerQuery("$theory\nqueryMode.", it, "[]", "[$it]", "[]") }
        argsUnd.forEach { answerQuery("$theory\nqueryMode.", it, "[]", "[]", "[$it]") }

        buildLabelSets(theory, parse(argsIn), parse(argsOut), parse(argsUnd))
    }

    @Test
    fun caminadaExample4() = protoTest(
        example4theory + "unrestrictedRebut.",
        listOf("wr", "m", "go", "b"),
        listOf(),
        listOf("-hw", "hw")
    )

    @Test
    fun caminadaExample4restricted() = protoTest(
        example4theory,
        listOf("wr", "-hw", "m", "hw", "go", "b"),
        listOf(),
        listOf(),
    )

    @Test
    fun caminadaExample4transposed() = protoTest(
        example4theory + """
            autoTransposition.
            unrestrictedRebut.
        """.trimIndent(),
        listOf("wr", "go"),
        listOf(),
        listOf("-m", "-hw", "-b", "m", "hw", "b")
    )

    @Test
    fun caminadaExample5() = protoTest(
        example5theory + "unrestrictedRebut.",
        listOf("e", "d", "c", "b", "a"),
        listOf("-c"),
        listOf()
    )

    @Test
    fun caminadaExample5restricted() = protoTest(
        example5theory,
        listOf("-c", "e", "d", "c", "b", "a"),
        listOf(),
        listOf()
    )

    @Test
    fun caminadaExample5transposed() = protoTest(
        example5theory + """
            autoTransposition.
            unrestrictedRebut.
        """.trimIndent(),
        listOf("d", "c", "a"),
        listOf("-c"),
        listOf("-e", "-b", "e", "b")
    )

    @Test
    fun caminadaExample6() = protoTest(
        example6theory + "unrestrictedRebut.",
        listOf("g", "f", "e", "d", "c", "b", "a"),
        listOf("-g"),
        listOf()
    )

    @Test
    fun caminadaExample6restricted() = protoTest(
        example6theory,
        listOf("-g", "g", "f", "e", "d", "c", "b", "a"),
        listOf(),
        listOf()
    )

    @Test
    fun caminadaExample6transposed() = protoTest(
        example6theory + """
            autoTransposition.
            unrestrictedRebut.
        """.trimIndent(),
        listOf("g", "d", "a"),
        listOf("-g"),
        listOf("-f", "-e", "-c", "-b", "f", "e", "c", "b")
    )

    @Test
    fun caminadaExample7transposed() = protoTest(
        example7theory + """
            argumentLabellingMode(grounded).
            autoTransposition.
            unrestrictedRebut.
        """.trimIndent(),
        listOf("g", "c", "b", "a"),
        listOf("-g"),
        listOf("-f", "-e", "-d", "f", "e", "d")
    )

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
