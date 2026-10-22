package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.PrologModule
import it.unibo.tuprolog.argumentation.core.libs.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.libs.graph.ids
import it.unibo.tuprolog.argumentation.core.libs.graph.labelled
import it.unibo.tuprolog.argumentation.core.libs.graph.replyList
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * Support module of the SCC-recursive semantics (complete, preferred, stable): their labellings are built one
 * strongly connected component at a time, the semantics only choosing which local labellings to keep.
 */
object SccLabelling : PrologModule("prolog.argumentation.graph.labelling.scc", "scc") {
    override val primitives: Map<Signature, Primitive>
        get() = listOf(UndecidedCandidates, Selection).associate { it.signature to it }

    override val prologRawTheory: String =
        """
        % label(+Accept, +Select): labellings built one strongly connected component at a time, in topological order.
        % The labels of a component only depend on the components before it, and are final once it is left.
        % Within a component the search forces IN its undecided arguments (grounded propagation labels the rest),
        % then keeps the local labellings that pass Accept and Select:
        %   Accept  admissible - the IN arguments are defended
        %           decided    - admissible, and no argument of the component left UND
        %   Select  all        - every accepted local labelling
        %           maximal    - the accepted ones with a maximal set of IN arguments in the component
        % Each solution leaves its labelling as the active context, released when the next one is computed.
        label(Accept, Select) :-
            cache_retract(scc_seen(_, _)),
            context_active(Start),
            grounded:::argumentLabelling,
            utils::sccs(Sccs),
            context_active(Root),
            (
                labelComponents(Accept, Select, Sccs, Root)
            ;
                context_checkout(Start),
                context_drop(Root),
                fail
            ).

        labelComponents(_, _, [], Branch) :-
            context_checkout(Branch).
        labelComponents(Accept, Select, [Scc|Sccs], Branch) :-
            context_last(Mark),
            findall(State-Ok, localLabelling(Accept, Scc, Branch, State, Ok), Explored),
            selection(Select, Scc, Explored, Selected),
            context_checkout(Branch),
            % every branch the local search created and did not select
            context_drop_after(Mark, Selected),
            member(Next, Selected),
            (
                labelComponents(Accept, Select, Sccs, Next)
            ;
                % a labelling is released once all its continuations are done (Branch by the previous component)
                Next \== Branch,
                context_checkout(Branch),
                context_drop(Next),
                fail
            ).

        % localLabelling(+Accept, +Scc, +Branch, -State, -Ok): State is Branch or a labelling reached from it by
        % forcing IN undecided arguments of Scc, Ok tells whether Accept holds for it

        localLabelling(Accept, Scc, Branch, State, Ok) :-
            reachable(Scc, Branch, State),
            context_checkout(State),
            (accepted(Accept, Scc) -> Ok = true ; Ok = false).

        accepted(admissible, _) :-
            admissible::admissible_set.
        accepted(decided, Scc) :-
            admissible::admissible_set,
            \+ (member(A, Scc), context_check(undId(A))).

        reachable(_, Branch, Branch).
        reachable(Scc, Branch, State) :-
            findall(I, context_check(Branch, inId(I)), Ins),
            undecided_candidates(Scc, Branch, Candidates),
            member(IdX, Candidates),
            utils::sort([IdX|Ins], Seed),
            \+ cache_check(scc_seen(Scc, Seed)),
            cache_assert(scc_seen(Scc, Seed)),
            context_check(Branch, clause(arg(IdX), argument(X))),
            context_branch(Branch, NewBranch),
            context_retract(und(_)),
            context_retract(undId(_)),
            context_assert(in(X)),
            context_assert(inId(IdX)),
            grounded::argumentLabelling,
            findall(R, context_check(inId(R)), Reached),
            utils::sort(Reached, Labelled),
            unexplored(Scc, Seed, Labelled),
            reachable(Scc, NewBranch, State).

        % Grounded propagation is monotone, so a labelling only depends on the set of arguments forced IN:
        % scc_seen(Scc, In) marks the labelling reached by forcing In within Scc as explored, whatever the order

        unexplored(_, Seed, Seed) :- !.
        unexplored(Scc, _, Labelled) :-
            \+ cache_check(scc_seen(Scc, Labelled)),
            cache_assert(scc_seen(Scc, Labelled)).
        """.trimIndent()
}

private interface SccPrimitive : Primitive {
    val signature: Signature
}

/**
 * undecided_candidates(+Scc, +Branch, -Ids): the UND arguments of Scc in Branch that can be forced IN, those not
 * attacked by an UND argument of a previous component (nothing left could defend them)
 */
private object UndecidedCandidates : SccPrimitive {
    override val signature = Signature("undecided_candidates", 3)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val scc = request.ids(0)
        val undecided = request.labelled("undId", request.arguments[1])
        val attackers = request.attackGraph().attackers
        val candidates =
            scc.filter { id ->
                id in undecided && attackers[id].orEmpty().none { it !in scc && it in undecided }
            }
        return sequenceOf(request.replyList(2, candidates) ?: request.replyFail())
    }
}

/**
 * selection(+Select, +Scc, +Explored, -Selected): the branches of the local labellings in Explored (a list of
 * Branch-Ok) kept by Select: all the accepted ones, or the accepted ones whose IN arguments in Scc are maximal
 */
private object Selection : SccPrimitive {
    override val signature = Signature("selection", 4)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val accepted =
            request
                .ids(2)
                .map { it.castToStruct() }
                .filter { (it[1] as? Atom)?.value == "true" }
                .map { it[0] }
        val selected =
            when (request.arguments[0].castToAtom().value) {
                "maximal" -> {
                    val scc = request.ids(1).toSet()
                    val local = accepted.associateWith { request.labelled("inId", it).intersect(scc) }
                    accepted.filter { state ->
                        local.values.none { other -> other != local[state] && other.containsAll(local.getValue(state)) }
                    }
                }
                else -> accepted
            }
        return sequenceOf(request.replyList(3, selected) ?: request.replyFail())
    }
}
