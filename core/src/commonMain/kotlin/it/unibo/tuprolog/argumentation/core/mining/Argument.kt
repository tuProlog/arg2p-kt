package it.unibo.tuprolog.argumentation.core.mining

import it.unibo.tuprolog.core.Cons
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.dsl.prolog
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.unify.Unificator
import kotlin.js.JsName

data class Support(val rules: List<String>, val conclusion: String, var identifier: String = "")

class Argument(
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

        private fun argTopRule(argument: List<Term>): String = argument[1].toString()

        private fun argConclusion(argument: List<Term>): String = argument[2].toString()

        private fun argRules(argument: List<Term>): List<String> {
            return (
                if (argument[0].isEmptyList) emptyList()
                else (argument[0] as Cons).toList()
                ).map { x -> x.toString() }
        }

        private fun argSupports(supports: List<Term>, argument: Term): List<Support> {
            return prolog {
                supports.map {
                    Unificator.default.mgu(it, tupleOf(X, argument))
                }.filter { it.isSuccess }.map { solution ->
                    (solution[X] as Cons)
                        .toList().let { arg -> Support(argRules(arg), argConclusion(arg)) }
                }.toList()
            }
        }

        private fun arguments(engine: Solver, label: String, arguments: Term?): Sequence<Argument> {
            if (arguments?.isEmptyList == true) return emptySequence()
            val supports = prolog{
                engine.solve("cache_check"("graph"(listOf(Var.anonymous(), Var.anonymous(), X)))).map { (it.substitution[X] as Cons).toList() }.first()
            }
            return (arguments as Cons).toSequence().map {
                (it as Cons).toList().let { arg ->
                    Argument(label, argTopRule(arg), argRules(arg), argConclusion(arg), argSupports(supports, it))
                }
            }
        }

        @JsName("mineArguments")
        fun mineArguments(engine: Solver): Sequence<Argument> {
            val arguments = prolog {
                engine.solve("cache_check"("labelling"(listOf(X, Y, Z))))
                    .filter { it.isYes }
                    .map { solution ->
                        sequenceOf(
                            arguments(engine, "in", solution.substitution[X]),
                            arguments(engine, "out", solution.substitution[Y]),
                            arguments(engine, "und", solution.substitution[Z])
                        ).flatten()
                    }
                    .firstOrNull()?.toList()
            } ?: emptyList()

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

            return arguments.asSequence()
        }
    }
}
