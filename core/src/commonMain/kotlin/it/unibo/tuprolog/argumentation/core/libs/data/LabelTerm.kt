package it.unibo.tuprolog.argumentation.core.libs.data

import it.unibo.tuprolog.argumentation.core.libs.PrologModule
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term

/**
 * The terms of the labels (module `label`): an argument labelled IN, OUT or UND is `in(Argument)`, `out(Argument)` or
 * `und(Argument)`, by its identifier `inId(Id)`, `outId(Id)` or `undId(Id)` (the native labellings), and a statement
 * `statIn(Statement)`, `statOut(Statement)` or `statUnd(Statement)`. A label is in, out or und. Kotlin builds them
 * through [argument], [identifier] and [statement].
 */
object LabelTerm : PrologModule("prolog.argumentation.data.label", "label") {
    override val prologRawTheory: String =
        """
        %! label(?Label)
        %  Label is a label: in, out or und.
        label(in).
        label(out).
        label(und).

        %! argument(?Label, ?Argument, ?Fact)
        %  Fact labels Argument with Label.
        argument(in, Argument, in(Argument)).
        argument(out, Argument, out(Argument)).
        argument(und, Argument, und(Argument)).

        %! identifier(?Label, ?Id, ?Fact)
        %  Fact labels the argument with identifier Id with Label.
        identifier(in, Id, inId(Id)).
        identifier(out, Id, outId(Id)).
        identifier(und, Id, undId(Id)).

        %! statement(?Label, ?Statement, ?Fact)
        %  Fact labels Statement with Label.
        statement(in, Statement, statIn(Statement)).
        statement(out, Statement, statOut(Statement)).
        statement(und, Statement, statUnd(Statement)).
        """.trimIndent()

    /** The labels: in, out and und */
    val labels: kotlin.collections.List<String> = listOf("in", "out", "und")

    /** The fact labelling [argument] with [label] */
    fun argument(
        label: String,
        argument: Term,
    ): Struct = Struct.of(label, argument)

    /** The fact labelling the argument with identifier [id] with [label] */
    fun identifier(
        label: String,
        id: Term,
    ): Struct = Struct.of("${label}Id", id)

    /** The fact labelling [statement] with [label] */
    fun statement(
        label: String,
        statement: Term,
    ): Struct = Struct.of("stat" + label.replaceFirstChar { it.uppercase() }, statement)
}
