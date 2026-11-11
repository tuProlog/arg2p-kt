package it.unibo.tuprolog.argumentation.engine.vm.compile

import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var

/**
 * The body of a clause as the compiler reads it: its goals, and the control constructs it compiles inline (a conjunction,
 * a disjunction, an if-then-else, a negation, a cut); a variable or a number as a goal is a call/1
 */
internal sealed class ClauseBody {
    class Goal(
        val term: Struct,
    ) : ClauseBody()

    class Conj(
        val items: List<ClauseBody>,
    ) : ClauseBody()

    class Disj(
        val term: Struct,
        val left: ClauseBody,
        val right: ClauseBody,
    ) : ClauseBody()

    class IfThenElse(
        val term: Struct,
        val condition: ClauseBody,
        val then: ClauseBody,
        val otherwise: ClauseBody,
    ) : ClauseBody()

    class Naf(
        val term: Struct,
        val goal: ClauseBody,
    ) : ClauseBody()

    object Cut : ClauseBody()

    object True : ClauseBody()

    companion object {
        /** The body [term] stands for */
        fun parse(term: Term): ClauseBody =
            when {
                term.isTrue -> ClauseBody.True
                term is Var -> ClauseBody.Goal(Struct.of("call", term))
                term is Atom && term.value == "!" -> ClauseBody.Cut
                term is Struct && term.arity == 2 && term.functor == "," -> {
                    val items = ArrayList<ClauseBody>()
                    var current: Term = term
                    while (current is Struct && current.arity == 2 && current.functor == ",") {
                        items.add(parse(current[0]))
                        current = current[1]
                    }
                    items.add(parse(current))
                    // (true is nothing to run: dropped, so that the goal before a trailing one is the last of the clause)
                    items.removeAll { it === ClauseBody.True }
                    when (items.size) {
                        0 -> ClauseBody.True
                        1 -> items[0]
                        else -> ClauseBody.Conj(items)
                    }
                }
                term is Struct && term.arity == 2 && term.functor == ";" -> {
                    val left = term[0]
                    if (left is Struct && left.arity == 2 && left.functor == "->") {
                        ClauseBody.IfThenElse(term, parse(left[0]), parse(left[1]), parse(term[1]))
                    } else {
                        ClauseBody.Disj(term, parse(left), parse(term[1]))
                    }
                }
                term is Struct && term.arity == 2 && term.functor == "->" -> {
                    ClauseBody.IfThenElse(term, parse(term[0]), parse(term[1]), parse(Atom.of("fail")))
                }
                term is Struct && term.arity == 1 && term.functor == "\\+" -> ClauseBody.Naf(term, parse(term[0]))
                term is Struct -> ClauseBody.Goal(term)
                // (a number as a goal: call/1 raises the type error when the clause runs)
                else -> ClauseBody.Goal(Struct.of("call", term))
            }
    }
}
