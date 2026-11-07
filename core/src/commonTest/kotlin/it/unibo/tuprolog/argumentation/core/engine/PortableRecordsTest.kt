package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.argumentation.core.engine.vm.Program
import it.unibo.tuprolog.argumentation.core.engine.vm.Symbols
import it.unibo.tuprolog.argumentation.core.engine.vm.payload
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertSame

/** Records written by one program and read by another: the same terms, up to the names of their variables */
class PortableRecordsTest {
    private val terms =
        listOf(
            "argument([[r1, f0], r1, [p(ann)], [[[f0], f0, [q(ann)], [], [[], [f0], []]]], [[r1], [r1], []]])",
            "f(X, g(X, Y), [a, b | T], Y)",
            "n(1.5, -2, 300000000, 123456789012345678901234567890, 'Quoted atom', \"text\")",
            "[[1, 2], [], -(p), f(-(q(X)), X)]",
            "only",
        ).map { Term.parse(it) }

    @Test
    fun theTermsAreReadInAnotherProgram() {
        val writer = Program()
        // the reader knows other symbols first, so that their numbers differ
        val reader = Program().also { p -> listOf("zeta", "only", "f", "g").forEach { p.atom(it) } }
        val records = terms.map { Record.of(it, writer) }
        val batch = PortableRecords.of(records)
        val read = batch.records(reader)
        assertEquals(terms.size, read.size)
        terms.zip(read).forEachIndexed { i, (term, record) ->
            assertEquals(Record.of(term, reader).key, record.key, "$term")
            // and as a term, read off the batch alone
            assertEquals(Record.of(term, reader).key, Record.of(batch.term(i), reader).key, "$term")
        }
    }

    // A part of a record as a record of its own, and a compound of records: what a node sends of an argument
    @Test
    fun partsAndCompounds() {
        val program = Program()
        val argument = Record.of(Term.parse("argument([[r1, f0], r1, [p(X, ann)], [X], [[], [f0], []]])"), program)
        val elements = argument.words[payload(argument.root) + 1]
        // the third element of the list: [p(X, ann)]
        var cell = payload(elements)
        repeat(2) { cell = payload(argument.words[cell + 1]) }
        val conclusion = argument.sub(argument.words[cell])
        assertEquals(Record.of(Term.parse("[p(Y, ann)]"), program).key, conclusion.key)
        val noted = Record.compound(program, program.functorWord("newConc", 2), listOf(Record.of(Term.parse("a"), program), conclusion))
        assertEquals(Record.of(Term.parse("newConc(a, [p(Z, ann)])"), program).key, noted.key)
    }

    @Test
    fun aRecordNamedAnotherWayKeepsItsName() {
        val writer = Program()
        val reader = Program()
        val record = Record.of(Term.parse("arg(7)"), writer).named(writer.functorWord("inId", 1))
        val read = PortableRecords.of(listOf(record)).records(reader).single()
        assertEquals("inId(7)", read.term.toString())
    }

    // Programs sharing their symbols (a session): the words read in place, the terms the same
    @Test
    fun theRecordsOfASessionAreReadInPlace() {
        val symbols = Symbols.shared(Symbols.of(null))
        val writer = Program(symbols = symbols)
        val reader = Program(symbols = symbols)
        val records = terms.map { Record.of(it, writer) } + Record.of(Term.parse("arg(7)"), writer).named(writer.functorWord("inId", 1))
        val batch = PortableRecords.of(records)
        val read = batch.records(reader)
        records.zip(read).forEachIndexed { i, (record, inReader) ->
            assertSame(record.words, inReader.words)
            assertEquals(record.key, inReader.key)
            if (i < terms.size) assertEquals(Record.of(batch.term(i), reader).key, inReader.key)
        }
        assertEquals("inId(7)", batch.term(records.size - 1).toString())
        // and by a program outside the session, through the tables of the batch
        val other = Program().also { p -> listOf("zeta", "only").forEach { p.atom(it) } }
        terms.zip(batch.records(other)).forEach { (term, record) -> assertEquals(Record.of(term, other).key, record.key, "$term") }
    }
}
