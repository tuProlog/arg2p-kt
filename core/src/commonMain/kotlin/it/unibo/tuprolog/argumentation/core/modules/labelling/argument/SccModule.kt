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
        %  Within a component the search forces IN its undecided arguments (grounded propagation labels the rest) up to
        %  the first admissible labellings, and labels what is left of the component above each of them as it labels the
        %  graph, one strongly connected component of it at a time; it keeps the local labellings that pass Accept and
        %  Select:
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
                solve(Accept, Select, top, Sccs, [], Root, State),
                context_checkout(State)
            ;
                context_checkout(Start),
                context_drop(Root),
                fail
            ).

        % solve(+Accept, +Select, +Mode, +Sccs, +Excluded, +Branch, -State): State labels the components Sccs from Branch,
        % none of Excluded IN. Mode is top for the graph, within for what is left of a component above an admissible
        % labelling of it (its states are released by the search of that component, not one by one)
        solve(_, _, _, [], _, Branch, Branch).
        solve(Accept, Select, Mode, [Scc | Sccs], Excluded, Branch, State) :-
            undecidedSccs(Scc, Branch, Pieces),
            solvePieces(Pieces, Accept, Select, Mode, Sccs, Excluded, Branch, State).

        % What is decided splits a component: what is left UND of it is labelled one strongly connected component at a
        % time, the first now (all UND, nothing to split), the others put before the components still to label, each
        % split again when its turn comes by what the ones before it decided
        solvePieces([], Accept, Select, Mode, Sccs, Excluded, Branch, State) :-
            solve(Accept, Select, Mode, Sccs, Excluded, Branch, State).
        solvePieces([Piece | Pieces], Accept, Select, Mode, Sccs, Excluded, Branch, State) :-
            append(Pieces, Sccs, Next),
            solveComponent(Accept, Select, Mode, Piece, Next, Excluded, Branch, State).

        solveComponent(Accept, Select, Mode, Scc, Sccs, Excluded, Branch, State) :-
            context_last(Mark),
            findall(Option-Ok, option(Accept, Select, Scc, Excluded, Branch, Option, Ok), Options),
            selection(Select, Scc, Options, Selected),
            context_checkout(Branch),
            % every branch the search of the component created and did not select
            context_drop_after(Mark, Selected),
            member(Next, Selected),
            (
                solve(Accept, Select, Mode, Sccs, Excluded, Next, State)
            ;
                % a labelling is released once all its continuations are done (Branch by the previous component)
                Mode == top,
                Next \== Branch,
                context_checkout(Branch),
                context_drop(Next),
                fail
            ).

        % option(+Accept, +Select, +Scc, +Excluded, +Branch, -State, -Ok): a local labelling of Scc from Branch, and
        % whether Accept holds for it: Branch itself, or one above an admissible labelling of Scc first reached from
        % Branch. The labellings above an admissible one are it and those of what is left of Scc, so the search does not
        % go on from it: what is left is split and labelled as the graph is
        option(Accept, _, Scc, _, Branch, Branch, Ok) :-
            context_checkout(Branch),
            ok(Accept, Scc, Ok).
        option(Accept, Select, Scc, Excluded, Branch, State, Ok) :-
            admissibleAbove(Scc, Excluded, Branch, Branch, Admissible, Mine),
            solve(Accept, Select, within, [Scc], Mine, Admissible, State),
            context_checkout(State),
            ok(Accept, Scc, Ok).

        ok(Accept, Scc, Ok) :-
            (accepted(Accept, Scc) -> Ok = true ; Ok = false).

        % (the IN arguments of the earlier components were checked with theirs: admissible::admissibleWithin)
        accepted(admissible, Scc) :-
            admissible::admissibleWithin(Scc).
        accepted(decided, Scc) :-
            admissible::admissibleWithin(Scc),
            \+ (member(A, Scc), graphStore::argumentLabelById(und, A)).

        % admissibleAbove(+Scc, +Excluded, +Start, +Branch, -Admissible, -Mine): Admissible is admissible on Scc, reached
        % from Branch by forcing IN undecided arguments of Scc with no admissible labelling on the way since Start; Mine is
        % Excluded and the candidates tried before on the way. Excluded are the candidates tried before in the branches
        % above, whose own branches found every admissible labelling above them that has them IN: none is IN below.
        % scc_seen(Start, In) marks a labelling of the search from Start as reached
        admissibleAbove(Scc, Excluded, Start, Branch, Admissible, Mine) :-
            undecidedCandidates(Scc, Branch, Excluded, Candidates),
            Candidates = [_ | _],
            findall(I, graphStore::argumentLabelByIdIn(Branch, in, I), Ins),
            candidate(Candidates, Excluded, IdX, Tried),
            utils::sortDescending([IdX | Ins], Seed),
            \+ cache_check(scc_seen(Start, Seed)),
            cache_assert(scc_seen(Start, Seed)),
            context_branch(Branch, NewBranch),
            % (Branch is a grounded fixpoint: the cascade of IdX alone, see grounded::propagateFrom)
            grounded::propagateFrom(IdX),
            force(Scc, Tried),
            graphStore::labelledIds(in, Reached),
            utils::sortDescending(Reached, Labelled),
            unexplored(Start, Seed, Labelled),
            (
                admissible::admissibleWithin(Scc)
            ->
                Admissible = NewBranch,
                Mine = Tried
            ;
                admissibleAbove(Scc, Tried, Start, NewBranch, Admissible, Mine)
            ).

        % candidate(+Candidates, +Excluded, -Id, -Mine): each candidate in turn, with those before it excluded
        candidate([Id | _], Excluded, Id, Excluded).
        candidate([Tried | Candidates], Excluded, Id, Mine) :-
            candidate(Candidates, [Tried | Excluded], Id, Mine).

        % force(+Scc, +Excluded): the defenders the IN arguments of Scc leave a single choice for forced IN, one at a
        % time, each followed by its propagation; fails when one is left with none (no admissible labelling is above the
        % state), or when an excluded argument is IN (what is above the state was found already)
        % (a component of one argument has nothing to force: its only candidate is an argument attacking itself)
        force([_], _) :- !.
        force(Scc, Excluded) :-
            forcing(Scc, Excluded, Next),
            forced(Next, Scc, Excluded).

        forced(none, _, _).
        forced(defender(D), Scc, Excluded) :-
            grounded::propagateFrom(D),
            force(Scc, Excluded).

        % A labelling only depends on the set of arguments forced IN and on what is excluded, and a state explored with
        % some excluded misses only labellings found from where they were tried: the labelling reached by forcing In is
        % marked as reached, whatever the order

        unexplored(_, Seed, Seed) :- !.
        unexplored(Start, _, Labelled) :-
            \+ cache_check(scc_seen(Start, Labelled)),
            cache_assert(scc_seen(Start, Labelled)).
        """.trimIndent()
}
