package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.libs.PrologModule
import it.unibo.tuprolog.argumentation.core.libs.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.libs.graph.conflictGraph
import it.unibo.tuprolog.argumentation.core.libs.graph.contextAssertAll
import it.unibo.tuprolog.argumentation.core.libs.graph.contextBranch
import it.unibo.tuprolog.argumentation.core.libs.graph.contextInstances
import it.unibo.tuprolog.argumentation.core.libs.graph.ids
import it.unibo.tuprolog.argumentation.core.libs.graph.labelled
import it.unibo.tuprolog.argumentation.core.libs.graph.replyList
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * The conflict-free labellings (module `conflictfree`, an argumentLabellingMode): a set of arguments IN, none
 * attacking another one, those they attack OUT and the rest UND. The other semantics take from it the arguments to
 * label and the labelling of a set.
 */
object ConflictFreeLabeller : PrologModule("prolog.argumentation.graph.labelling.conflictfree", "conflictfree") {
    override val primitives: Map<Signature, Primitive>
        get() = mapOf(ConflictFreeSubset.signature to ConflictFreeSubset, LabelSet.signature to LabelSet)

    override val prologRawTheory: String =
        """
        %! locality(-Locality)
        %  Query mode: the label of an argument only depends on the arguments it depends on (directionality)
        locality(directional).

        %! argumentLabelling
        %  Each conflict-free labelling in turn, left as the active context.
        argumentLabelling :-
            context_active(Branch),
            candidates(Args, Forced),
            conflict_free_subset(Args, Forced, Set),
            labelSet(Branch, Set).

        %! candidates(-Args, -Forced)
        %  The arguments to label, and those among them already IN (kept IN)
        candidates(Args, Forced) :-
            findall(A, context_check(clause(arg(A), _)), Args),
            findall(A, (member(A, Args), context_check(inId(A))), Forced).

        %! labelSet(+Branch, +Set)
        %  In a new branch of Branch, Set IN, the arguments it attacks OUT, the rest UND;
        %  the branch is released on backtracking
        labelSet(Branch, Set) :-
            label_set(Branch, Set),
            context_active(Labelling),
            (
                true
            ;
                context_checkout(Branch),
                context_drop(Labelling),
                fail
            ).
        """.trimIndent()
}

/**
 * conflict_free_subset(+Args, +Forced, -Set): the subsets of Args including Forced where no argument attacks another
 * one (itself included), each once. The search only adds arguments compatible with those already chosen, so every
 * branch ends in a solution: the cost grows with the number of sets, not with the subsets of Args.
 */
object ConflictFreeSubset : Primitive {
    val signature: Signature = Signature("conflict_free_subset", 3)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val args = request.ids(0)
        val forced = request.ids(1).toSet()
        val graph = request.conflictGraph()

        fun subsets(
            i: Int,
            chosen: List<Term>,
        ): Sequence<List<Term>> =
            sequence {
                if (i == args.size) {
                    yield(chosen)
                    return@sequence
                }
                val a = args[i]
                if (!graph.selfAttacking(a) && chosen.none { graph.conflict(a, it) }) yieldAll(subsets(i + 1, chosen + a))
                if (a !in forced) yieldAll(subsets(i + 1, chosen))
            }
        return subsets(0, emptyList()).mapNotNull { request.replyList(2, it) }
    }
}

/**
 * label_set(+Branch, +Ids): in a new branch of Branch (left active), the arguments of Ids IN, the unlabelled
 * arguments attacked by an IN one OUT, and the other unlabelled ones UND, asserted with a single context_assert_all
 */
object LabelSet : Primitive {
    val signature: Signature = Signature("label_set", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
        arg2pScope {
            if (!request.contextBranch(request.arguments[0])) return@arg2pScope sequenceOf(request.replyFail())
            val set = request.ids(1).toSet()
            val arguments =
                request
                    .contextInstances("clause"("arg"(`_`), "argument"(`_`)))
                    .map { it[0].castToStruct()[0] to it[1].castToStruct()[0] }
            val labelled = request.labelled("inId") + request.labelled("outId") + request.labelled("undId")
            val newIn = arguments.filter { (id, _) -> id in set && id !in labelled }
            val inIds = request.labelled("inId") + newIn.map { it.first }
            val attackers = request.attackGraph().attackers
            val (out, und) =
                arguments
                    .filter { (id, _) -> id !in labelled && id !in inIds }
                    .partition { (id, _) -> attackers[id].orEmpty().any { it in inIds } }

            fun facts(
                label: String,
                args: List<Pair<Term, Term>>,
            ) = args.flatMap { (id, x) -> listOf(label(x), "${label}Id"(id)) }
            val all = facts("in", newIn) + facts("out", out) + facts("und", und)
            request.contextAssertAll(all)
            sequenceOf(request.replyWith(true))
        }
}
