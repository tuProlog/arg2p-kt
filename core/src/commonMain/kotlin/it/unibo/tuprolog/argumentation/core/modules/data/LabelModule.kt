package it.unibo.tuprolog.argumentation.core.modules.data

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The terms of the labels (module `label`): an argument labelled IN, OUT or UND is `in(Argument)`, `out(Argument)` or
 * `und(Argument)`, by its identifier `inId(Id)`, `outId(Id)` or `undId(Id)` (the native labellings), and a statement
 * `statIn(Statement)`, `statOut(Statement)` or `statUnd(Statement)`. A label is in, out or und. Kotlin builds them
 * through [argument], [identifier] and [statement].
 */
object LabelModule : PrologModule("prolog.argumentation.data.label", "label") {
    override val prologRawTheory: String =
        // language=Prolog
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
}
