// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
package it.unibo.tuprolog.argumentation.core.engine.stdlib.primitive

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.exception.LogicError
import it.unibo.tuprolog.solve.exception.ResolutionException
import it.unibo.tuprolog.solve.exception.error.MessageError
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.solve.primitive.UnaryPredicate

/**
 * ISO `throw/1`: raises its argument as a [ResolutionException], to be looked for by an enclosing `catch/3` as
 * the state machine climbs the execution-context stack in `StateException`.
 *
 * If the argument is an `error(Type, Extra)` (or `error(Type)`) compound, it is wrapped into a [LogicError] of
 * the given `it.unibo.tuprolog.solve.exception.error` `Type`; any other term is wrapped into a generic
 * [MessageError] instead.
 *
 * @throws it.unibo.tuprolog.solve.exception.error.InstantiationError if the argument is an unbound variable.
 */
object Throw : UnaryPredicate<ExecutionContext>("throw") {
    override val help: String =
        """
        `throw(+Ball)`

        Raises `Ball` as an exception: resolution unwinds to the innermost enclosing `catch/3` whose catcher unifies with `Ball`, or halts the query if none does. A ball shaped like `error(Type, Context)` (or `error(Type)`) is raised as the standard error of that `Type`, any other term as a user-defined exception carrying it. `Ball` must be instantiated, otherwise an instantiation error is raised instead.

        **Examples**

        ```prolog
        ?- catch(throw(my_error), E, true).
        E = my_error.

        ?- throw(error(type_error(integer, a), my_context)).
        throws error(type_error(integer, a), _).

        ?- throw(_).
        throws error(instantiation_error, _).
        ```
        """.trimIndent()

    private fun handleError(
        context: ExecutionContext,
        error: Term,
    ): ResolutionException =
        when {
            // arg2p: an error whose type is not a structure (error(1), error(_, C)) is raised as any other ball
            error is Struct && error.functor == "error" && error.arity in 1..2 && error[0] is Struct -> {
                try {
                    LogicError.of(
                        context = context,
                        type = error[0] as Struct,
                        extraData = if (error.arity > 1) error[1] else null,
                    )
                } catch (_: IllegalArgumentException) {
                    // arg2p: a type 2P-Kt does not know the arguments of (a domain_error of a domain outside ISO, as
                    // those of the languages of the profiles) is raised as any other ball, its message kept
                    MessageError.of(error, context)
                }
            }
            else -> MessageError.of(error, context)
        }

    override fun Solve.Request<ExecutionContext>.computeAll(first: Term): Sequence<Solve.Response> =
        sequenceOf(
            ensuringAllArgumentsAreInstantiated()
                .replyException(handleError(context, arguments[0])),
        )
}
