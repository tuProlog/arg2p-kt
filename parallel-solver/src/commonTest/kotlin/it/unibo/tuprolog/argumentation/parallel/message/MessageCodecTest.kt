package it.unibo.tuprolog.argumentation.parallel.message

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

class MessageCodecTest {
    // texts with the characters of the encoding itself, digits and colons, and lines
    private val text = "12:ab: 'x:y'(3).\nnext"

    // an argument and a statement, with the operators of arg2p
    private val operators = Arg2pSolver.default().operators()
    private val argument = Term.parse("[[r1, f1], r1, ['-'(b)], [a, ~(c)], [[r1], [r1], [f1]]]", operators)
    private val statement = Term.parse("'-'(p('x:y', 12))", operators)

    @Test
    fun everyMessageIsDecodedAsItWasEncoded() {
        val messages =
            listOf(
                Reset,
                Load(text),
                Add(text),
                RequireEvaluation(text, "client_1"),
                EvaluationResponse(listOf(Response(text, listOf(text, ""))), emptyList(), listOf(Response("b", emptyList()))),
                Subscribe(2, listOf(statement, argument)),
                Build(listOf(Interest(0, listOf(statement)), Interest(1, emptyList()))),
                Notify(1, listOf(SharedArgument("1:3", argument))),
                Publish(1, listOf(SharedArgument("1:3", argument, listOf("0:1", "2:4")), SharedArgument("1:4", statement))),
                Ack(-1),
                Attack,
                Wanted(0, listOf(statement)),
                Candidates(2, listOf(SharedArgument("2:7", argument))),
                AttacksFound(3, listOf(DirectAttack("rebut", "0:1", "3:12"))),
            )
        messages.forEach { assertEquals(it, MessageCodec.decode(MessageCodec.encode(it))) }
    }
}
