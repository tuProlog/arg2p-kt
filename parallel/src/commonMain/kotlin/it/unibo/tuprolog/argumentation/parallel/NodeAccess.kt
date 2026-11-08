package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.message.AnswerResponse
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import it.unibo.tuprolog.argumentation.parallel.message.SolutionResponse

/**
 * What a parallel solver reaches of its node: the node itself, in its thread ([LocalAccess]), or a node on another thread (a
 * worker of JavaScript, see NodeProxy), its replies given to the callbacks as they come
 */
internal interface NodeAccess {
    var flags: Flags

    fun load(theory: String)

    fun add(clause: String)

    fun reset()

    val evaluation: EvaluationStats?

    fun solve(
        statement: String,
        reply: (EvaluationResponse) -> Unit,
    )

    fun query(
        goal: String,
        reply: (SolutionResponse) -> Unit,
    )

    fun next(
        run: String,
        reply: (SolutionResponse) -> Unit,
    )

    fun answer(
        query: String,
        reply: (AnswerResponse) -> Unit,
    )

    fun stop()
}

/** [node], in the thread of its parallel solver */
internal class LocalAccess(
    internal val node: ParallelNode,
) : NodeAccess {
    override var flags: Flags
        get() = node.flags
        set(value) {
            node.flags = value
        }

    override fun load(theory: String) = node.load(theory)

    override fun add(clause: String) = node.add(clause)

    override fun reset() = node.reset()

    override val evaluation: EvaluationStats? get() = node.evaluation

    override fun solve(
        statement: String,
        reply: (EvaluationResponse) -> Unit,
    ) = node.solve(statement, reply)

    override fun query(
        goal: String,
        reply: (SolutionResponse) -> Unit,
    ) = node.query(goal, reply)

    override fun next(
        run: String,
        reply: (SolutionResponse) -> Unit,
    ) = node.next(run, reply)

    override fun answer(
        query: String,
        reply: (AnswerResponse) -> Unit,
    ) = node.answer(query, reply)

    override fun stop() = node.stop()
}
