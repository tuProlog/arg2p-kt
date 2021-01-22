package it.unibo.tuprolog.argumentation.ui.gui.graph

import it.unibo.tuprolog.core.Cons
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.dsl.prolog
import it.unibo.tuprolog.solve.Solver

class Argument(val label: String, val topRule: String, val rules: List<String>, val conclusion: String) {

    private var subargs: List<String> = emptyList()
    var identifier = ""

    val descriptor: String
        get() = "$identifier : " + (
            if (topRule == "none") rules.firstOrNull() ?: "" else {
                subargs.plus(topRule)
                    .reduce { a: String, b: String -> "$a,$b" }
            }
            ) + " : " + conclusion

    private fun addSubarg(subarg: String) {
        this.subargs = this.subargs.plus(subarg)
    }

    companion object {

        private fun arguments(label: String, arguments: Term?): Sequence<Argument> {
            if (arguments?.isEmptyList == true) return emptySequence()
            return (arguments as Cons).toSequence().map {
                (it as Cons).toList().let { arg ->
                    Argument(label, arg[1].toString(), (arg[0] as Cons).toList().map { x -> x.toString() }, arg[2].toString())
                }
            }
        }

        fun mineArguments(engine: Solver): Sequence<Argument> {
            val arguments = prolog {
                engine.solve("argsLabelling"(X, Y, Z))
                    .filter { it.isYes }
                    .map { solution ->
                        sequenceOf(
                            arguments("in", solution.substitution[X]),
                            arguments("out", solution.substitution[Y]),
                            arguments("und", solution.substitution[Z])
                        ).flatten()
                    }
                    .firstOrNull()?.toList()
            } ?: emptyList()

            arguments
                .sortedBy { it.rules.size }
                .mapIndexed { index, arg ->
                    arg.identifier = "A$index"
                    arg
                }
                .forEach { arg ->
                    arguments
                        .filter { a -> a.identifier != arg.identifier }
                        .takeIf { it.isNotEmpty() }
                        ?.reduce { a: Argument, b: Argument ->
                            if (arg.rules.containsAll(b.rules) && b.rules.size >= a.rules.size) b else a
                        }?.let { sub ->
                            if (arg.rules.size > 1) arg.addSubarg(sub.identifier)
                        }
                    // arg.conclusion = prolog {
                    //     engine.solve("rule"(listOf(arg.topRule, Var.ANONYMOUS_VAR_NAME, X)))
                    //         .filter { it.isYes }
                    //         .map { it.substitution[X].toString() }
                    //         .firstOrNull()
                    // } ?: {
                    //     engine.solve("rule"(listOf(arg.topRule, Var.ANONYMOUS_VAR_NAME, X)))
                    //         .filter { it.isYes }
                    //         .map { it.substitution[X].toString() }
                    //         .firstOrNull()
                    // }
                }

            return arguments.asSequence()
        }
    }
}
