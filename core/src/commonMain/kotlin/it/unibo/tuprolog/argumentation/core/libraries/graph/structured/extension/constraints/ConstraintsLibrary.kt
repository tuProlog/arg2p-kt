package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.constraints

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.constraints.flags.OpenConclusions
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.constraints.primitives.LinearConstraint
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.constraints.primitives.LinearSatisfiable
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.constraints.prolog.THEORY
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The arguments holding for sets of values (module `constraints`, a graph extension, required as well by temporal).
 *
 * An argument may conclude an open statement (with variables): it holds for every value its constraints allow, the
 * `{ C }` premises of its rules and those of its sub-arguments still open. `holdsAt(f, T)` with `{ T > 1 }` holds for
 * every T after 1. The constraints decided are the linear comparisons over numbers (`<`, `=<`, `>`, `>=`, `=:=`, `=`
 * between sums of numbers and variables, products and quotients by a number), the numbers dense, whatever their
 * variables: the simplex on exact rationals decides whether they hold together. Any other constraint is a check, as
 * `prolog(C)`, run where its rule is used.
 *
 * Where an attack holds for some of the values of an argument only, the graph step splits the argument into pieces: the
 * argument with the attacker's constraints added (attacked), and with them negated one at a time (free), each piece
 * resting on the pieces of its sub-arguments holding for its values; the argument split stays, attacking and attacked, for all its values.
 * Supports and queries read the pieces admitting their values, so that the whole graph and query mode answer alike. An
 * attack whose region needs variables of the attacker that the attacked argument does not have holds for all values.
 * Nothing is removed from the graph. The base modules call the extension through its hooks (see parser::hook/2), never
 * by name. See `prolog/theory.pl` and `language/theory.pl`.
 */
object ConstraintsLibrary : PrologLibrary("constraints") {
    override val baseFlags: Iterable<ArgsFlag>
        get() = listOf(OpenConclusions)

    // (the flag a solver sets for the rules of a theory written out, those of temporal among them: see parser::extension/1)
    override val extensionFlag: ArgsFlag
        get() = OpenConclusions

    override val prologRawTheory: String = THEORY

    override val primitives: Map<Signature, Primitive>
        get() = listOf(LinearConstraint, LinearSatisfiable).associateBy { it.signature }
}
