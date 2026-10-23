package it.unibo.tuprolog.argumentation.core.modules.labelling.argument

import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.labelling.argument.Selection
import it.unibo.tuprolog.argumentation.core.primitives.labelling.argument.UndecidedCandidates
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * Support module of the SCC-recursive semantics (complete, preferred, stable): their labellings are built one
 * strongly connected component at a time, the semantics only choosing which local labellings to keep.
 */
object SccModule : PrologModule("prolog.argumentation.graph.labelling.scc", "scc") {
    override val primitives: Map<Signature, Primitive>
        get() = listOf(UndecidedCandidates, Selection).associate { it.signature to it }

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! label(+Accept, +Select)
        %  Labellings built one strongly connected component at a time, in topological order.
        %  The labels of a component only depend on the components before it, and are final once it is left.
        %  Within a component the search forces IN its undecided arguments (grounded propagation labels the rest),
        %  then keeps the local labellings that pass Accept and Select:
        %    Accept  admissible - the IN arguments are defended
        %            decided    - admissible, and no argument of the component left UND
        %    Select  all        - every accepted local labelling
        %            maximal    - the accepted ones with a maximal set of IN arguments in the component
        %  Each solution leaves its labelling as the active context, released when the next one is computed.
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
            admissible::admissibleSet.
        accepted(decided, Scc) :-
            admissible::admissibleSet,
            \+ (member(A, Scc), graphStore::argumentLabelById(und, A)).

        reachable(_, Branch, Branch).
        reachable(Scc, Branch, State) :-
            findall(I, graphStore::argumentLabelByIdIn(Branch, in, I), Ins),
            undecidedCandidates(Scc, Branch, Candidates),
            member(IdX, Candidates),
            utils::sort([IdX|Ins], Seed),
            \+ cache_check(scc_seen(Scc, Seed)),
            cache_assert(scc_seen(Scc, Seed)),
            graphStore::argumentByIdIn(Branch, IdX, X),
            context_branch(Branch, NewBranch),
            graphStore::unlabelArguments(und),
            graphStore::labelArgument(in, X, IdX),
            grounded::argumentLabelling,
            graphStore::labelledIds(in, Reached),
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
