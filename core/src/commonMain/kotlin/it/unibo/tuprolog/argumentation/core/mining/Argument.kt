package it.unibo.tuprolog.argumentation.core.mining

import it.unibo.tuprolog.core.Cons
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.dsl.prolog
import it.unibo.tuprolog.solve.Solver
import kotlin.js.JsName

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
                    Argument(
                        label,
                        arg[1].toString(),
                        (if (arg[0].isEmptyList) emptyList() else (arg[0] as Cons).toList())
                            .map { x -> x.toString() },
                        arg[2].toString()
                    )
                }
            }
        }

        @JsName("mineArguments")
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
                        .filter { a -> a.identifier != arg.identifier && a.rules.isNotEmpty() }
                        .takeIf { it.isNotEmpty() }
                        ?.reduce { a: Argument, b: Argument ->
                            if (arg.rules.containsAll(b.rules) && b.rules.size >= a.rules.size) b else a
                        }?.let { sub ->
                            if (arg.rules.size > 1) arg.addSubarg(sub.identifier)
                        }
                }

            return arguments.asSequence()
        }
    }
}
