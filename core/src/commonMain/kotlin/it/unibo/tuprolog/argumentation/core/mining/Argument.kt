package it.unibo.tuprolog.argumentation.core.mining

import it.unibo.tuprolog.core.Cons
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.dsl.prolog
import it.unibo.tuprolog.solve.Solver
import kotlin.js.JsName

data class Support(val rules: List<String>, val conclusion: String, var identifier: String = "")

class Argument(
    val term: Term,
    val label: String,
    val topRule: String,
    val rules: List<String>,
    val conclusion: String,
    val supports: List<Support>,
    var identifier: String = ""
) {

    val descriptor: String
        get() = "$identifier : " + (
            if (topRule == "none") rules.firstOrNull() ?: "" else {
                supports.map { it.identifier }.plus(topRule)
                    .reduce { a: String, b: String -> "$a,$b" }
            }
            ) + " : " + conclusion

    companion object {

        private fun argumentAsList(argument: Term?): List<Term> = (argument as Cons).toList()

        private fun argTopRule(argument: List<Term>): String = argument[1].toString()

        private fun argConclusion(argument: List<Term>): String = argument[2].toString()

        private fun argRules(argument: List<Term>): List<String> {
            return (
                if (argument[0].isEmptyList) emptyList()
                else (argument[0] as Cons).toList()
                ).map { x -> x.toString() }
        }

        private fun argSupports(engine: Solver, context: Int, argument: Term): Sequence<Support> =
            prolog { engine.solve("context_check"(context, "support"(X, argument)))
                .filter { it.isYes }
                .map { it.substitution[X]!! }
                .map { solution ->
                    Support(
                        argRules(argumentAsList(solution)),
                        argConclusion(argumentAsList(solution))
                    )
                }
            }

        private fun argLabel(engine: Solver, context: Int, argument: Term): String {

            fun checkFunctor(functor: String) =
                prolog {
                    engine.solve("context_check"(context, functor(argument)))
                        .filter { it.isYes }
                        .map { functor }
                        .firstOrNull()
                }

            return checkFunctor("in") ?:
                checkFunctor("out") ?: "und"
        }

        @JsName("mineArguments")
        fun mineArguments(context: Int, engine: Solver): List<Argument> {
            val arguments = prolog {
                engine.solve("context_check"(context, "argument"(X)))
                    .filter { it.isYes }
                    .map { it.substitution[X]!! }
                    .map { solution ->
                        Argument(
                            solution,
                            argLabel(engine, context, solution),
                            argTopRule(argumentAsList(solution)),
                            argRules(argumentAsList(solution)),
                            argConclusion(argumentAsList(solution)),
                            argSupports(engine, context, solution).toList()
                        )
                    } }.toList()

            arguments
                .sortedWith(compareBy({ it.supports.size }, { it.rules.size }, { it.conclusion }, { it.topRule }))
                .mapIndexed { index, arg ->
                    arg.identifier = "A$index"
                    arg
                }
                .forEach { arg ->
                    arg.supports
                        .forEach { support ->
                            support.identifier = arguments.firstOrNull {
                                it.rules == support.rules &&
                                    it.conclusion == support.conclusion
                            }!!.identifier
                        }
                }

            return arguments
        }
    }
}
