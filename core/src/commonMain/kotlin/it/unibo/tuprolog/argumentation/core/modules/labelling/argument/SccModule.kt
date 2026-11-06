package it.unibo.tuprolog.argumentation.core.modules.labelling.argument

import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.labelling.argument.Forcing
import it.unibo.tuprolog.argumentation.core.primitives.labelling.argument.Selection
import it.unibo.tuprolog.argumentation.core.primitives.labelling.argument.UndecidedCandidates
import it.unibo.tuprolog.argumentation.core.primitives.labelling.argument.UndecidedSccs
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * Support module of the SCC-recursive semantics (complete, preferred, stable): their labellings are built one
 * strongly connected component at a time, the semantics only choosing which local labellings to keep.
 */
object SccModule : PrologModule("prolog.argumentation.graph.labelling.scc", "scc") {
    override val primitives: Map<Signature, Primitive>
        get() = listOf(UndecidedCandidates, UndecidedSccs, Selection, Forcing).associate { it.signature to it }

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
            undecidedSccs(Scc, Branch, Pieces),
            labelPieces(Pieces, Accept, Select, Sccs, Branch).

        % What the earlier components decided splits a component: what is left UND of it is searched one strongly
        % connected component at a time, each split again on arrival by what the ones before it decided
        labelPieces([], Accept, Select, Sccs, Branch) :-
            labelComponents(Accept, Select, Sccs, Branch).
        labelPieces([Scc], Accept, Select, Sccs, Branch) :-
            labelComponent(Accept, Select, Scc, Sccs, Branch).
        labelPieces([Piece, Other | Pieces], Accept, Select, Sccs, Branch) :-
            append([Piece, Other | Pieces], Sccs, Next),
            labelComponents(Accept, Select, Next, Branch).

        labelComponent(Accept, Select, Scc, Sccs, Branch) :-
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

        % (the IN arguments of the earlier components were checked with theirs: admissible::admissibleWithin)
        accepted(admissible, Scc) :-
            admissible::admissibleWithin(Scc).
        accepted(decided, Scc) :-
            admissible::admissibleWithin(Scc),
            \+ (member(A, Scc), graphStore::argumentLabelById(und, A)).

        reachable(_, Branch, Branch).
        reachable(Scc, Branch, State) :-
            undecidedCandidates(Scc, Branch, Candidates),
            Candidates = [_ | _],
            findall(I, graphStore::argumentLabelByIdIn(Branch, in, I), Ins),
            member(IdX, Candidates),
            utils::sortDescending([IdX|Ins], Seed),
            \+ cache_check(scc_seen(Scc, Seed)),
            cache_assert(scc_seen(Scc, Seed)),
            context_branch(Branch, NewBranch),
            % (Branch is a grounded fixpoint: the cascade of IdX alone, see grounded::propagateFrom)
            grounded::propagateFrom(IdX),
            force(Scc),
            graphStore::labelledIds(in, Reached),
            utils::sortDescending(Reached, Labelled),
            unexplored(Scc, Seed, Labelled),
            reachable(Scc, NewBranch, State).

        % force(+Scc): the defenders the IN arguments of Scc leave a single choice for forced IN, one at a time, each
        % followed by its propagation; fails when one is left with none (no admissible labelling is above the state)
        % (a component of one argument has nothing to force: its only candidate is an argument attacking itself)
        force([_]) :- !.
        force(Scc) :-
            forcing(Scc, Next),
            forced(Next, Scc).

        forced(none, _).
        forced(defender(D), Scc) :-
            grounded::propagateFrom(D),
            force(Scc).

        % Grounded propagation is monotone, so a labelling only depends on the set of arguments forced IN:
        % scc_seen(Scc, In) marks the labelling reached by forcing In within Scc as explored, whatever the order

        unexplored(_, Seed, Seed) :- !.
        unexplored(Scc, _, Labelled) :-
            \+ cache_check(scc_seen(Scc, Labelled)),
            cache_assert(scc_seen(Scc, Labelled)).
        """.trimIndent()
}
