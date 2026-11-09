package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.constraints

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.constraints.flags.OpenConclusions
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.constraints.prolog.THEORY

/**
 * The constraints on the variables of the arguments, and the arguments holding for them (module `constraints`).
 *
 * A premise `{ C }` of a rule is checked as `prolog(C)` once its variables are bound, and kept on the argument while they
 * are not. An argument keeps the constraints of its sub-arguments still open among its premises, and a premise or a
 * conflict binding its variables is admitted only while all of them may hold together. The constraints decided are those
 * on a single variable: comparisons with a number (`<`, `=<`, `>`, `>=`, `=:=`, `=\=`, the numbers dense), membership
 * (`member(X, [a, b])`), equality and exclusion of a term (`X = a`, `X \= a`), and their conjunctions. A constraint
 * relating two variables waits for one of them to be bound. See `language/theory.pl`.
 *
 * The arguments holding for sets of values (a graphExtension, on as well with a language whose arguments conclude open
 * statements, as temporal, see opensConclusions/0 in the language part): without
 * it no argument concludes an open statement (with free variables); with it one may, for the values its variables may
 * take (constrained by its `{ }` premises): `holdsAt(f, T)` with `{ T > 1 }` holds for every
 * T after 1. Its instance for some of them is an argument of its own, made of the same rules, its sub-arguments
 * instantiated alike where the values flow:
 * - an argument using it for some values (`alarm(5)` from `holdsAt(f, 5)`) rests on its instance for them, saved
 *   when the graph is changed (the builder saves the support as bound);
 * - a conflict binding its variables (`-holdsAt(f, 7)`) attacks it, and its instance for those values, which the graph
 *   shows.
 *
 * An argument attacked for some of its values is attacked for all of them (it states them all), and its instances for
 * the others are not: each instance stands or falls on its own values. Nothing is removed from the graph.
 */
object ConstraintsLibrary : PrologLibrary("constraints") {
    override val baseFlags: Iterable<ArgsFlag>
        get() = listOf(OpenConclusions)

    override val prologRawTheory: String = THEORY
}
