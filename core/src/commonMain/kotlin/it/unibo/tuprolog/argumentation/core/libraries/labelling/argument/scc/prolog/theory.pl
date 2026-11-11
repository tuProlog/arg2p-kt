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
    label(Accept, Select, grounded).

%! label(+Accept, +Select, +Closure)
%  The same, the labellings closed by Closure:
%    grounded - an argument forced IN with what it makes IN by grounded propagation, from the grounded
%               labelling (complete labellings: what the IN arguments defend is IN)
%    none     - an argument forced IN with only what it attacks OUT, from the labelling with nothing more IN
%               (admissible labellings)
label(Accept, Select, Closure) :-
    cacheRetract(sccSeen(_, _)),
    contextActive(Start),
    root(Closure, Start),
    store::sccs(Sccs),
    contextActive(Root),
    (
        labelSccs(Accept, Select, Closure, top, Sccs, [], Root, State),
        contextCheckout(State)
    ;
        contextCheckout(Start),
        contextDrop(Root),
        fail
    ).

root(grounded, _) :-
    grounded:::argumentLabelling.
root(none, Start) :-
    labelSets::labelSet(Start, []).

% forceIn(+Closure, +Id): Id forced IN, and what follows from it by Closure
forceIn(grounded, Id) :-
    % (the state is a grounded fixpoint: the cascade of Id alone, see grounded::propagateFrom)
    grounded::propagateFrom(Id).
forceIn(none, Id) :-
    labelIn(Id).

% labelSccs(+Accept, +Select, +Closure, +Mode, +Sccs, +Excluded, +Branch, -State): State labels the components Sccs
% from Branch, none of Excluded IN. Mode is top for the graph, within for what is left of a component above an
% admissible labelling of it (its states are released by the search of that component, not one by one)
labelSccs(_, _, _, _, [], _, Branch, Branch).
labelSccs(Accept, Select, Closure, Mode, [Scc | Sccs], Excluded, Branch, State) :-
    undecidedSccs(Scc, Branch, Pieces),
    solvePieces(Pieces, Accept, Select, Closure, Mode, Sccs, Excluded, Branch, State).

% What is decided splits a component: what is left UND of it is labelled one strongly connected component at a
% time, the first now (all UND, nothing to split), the others put before the components still to label, each
% split again when its turn comes by what the ones before it decided
solvePieces([], Accept, Select, Closure, Mode, Sccs, Excluded, Branch, State) :-
    labelSccs(Accept, Select, Closure, Mode, Sccs, Excluded, Branch, State).
solvePieces([Piece | Pieces], Accept, Select, Closure, Mode, Sccs, Excluded, Branch, State) :-
    append(Pieces, Sccs, Next),
    solveComponent(Accept, Select, Closure, Mode, Piece, Next, Excluded, Branch, State).

solveComponent(Accept, Select, Closure, Mode, Scc, Sccs, Excluded, Branch, State) :-
    contextLast(Mark),
    findall(Option-Ok, option(Accept, Select, Closure, Scc, Excluded, Branch, Option, Ok), Options),
    selection(Select, Scc, Options, Selected),
    contextCheckout(Branch),
    % every branch the search of the component created and did not select
    contextDropAfter(Mark, Selected),
    member(Next, Selected),
    (
        labelSccs(Accept, Select, Closure, Mode, Sccs, Excluded, Next, State)
    ;
        % a labelling is released once all its continuations are done (Branch by the previous component)
        Mode == top,
        Next \== Branch,
        contextCheckout(Branch),
        contextDrop(Next),
        fail
    ).

% option(+Accept, +Select, +Closure, +Scc, +Excluded, +Branch, -State, -Ok): a local labelling of Scc from Branch,
% and whether Accept holds for it: Branch itself, or one above an admissible labelling of Scc first reached from
% Branch. The labellings above an admissible one are it and those of what is left of Scc, so the search does not
% go on from it: what is left is split and labelled as the graph is
option(Accept, _, _, Scc, _, Branch, Branch, Ok) :-
    contextCheckout(Branch),
    ok(Accept, Scc, Ok).
option(Accept, Select, Closure, Scc, Excluded, Branch, State, Ok) :-
    admissibleAbove(Closure, Scc, Excluded, Branch, Branch, Admissible, Mine),
    labelSccs(Accept, Select, Closure, within, [Scc], Mine, Admissible, State),
    contextCheckout(State),
    ok(Accept, Scc, Ok).

ok(Accept, Scc, Ok) :-
    (componentAccepted(Accept, Scc) -> Ok = true ; Ok = false).

% (the IN arguments of the earlier components were checked with theirs: labelSets::admissibleWithin)
componentAccepted(admissible, Scc) :-
    labelSets::admissibleWithin(Scc).
componentAccepted(decided, Scc) :-
    labelSets::admissibleWithin(Scc),
    \+ (member(A, Scc), store::argumentLabelById(und, A)).

% admissibleAbove(+Closure, +Scc, +Excluded, +Start, +Branch, -Admissible, -Mine): Admissible is admissible on
% Scc, reached from Branch by forcing IN undecided arguments of Scc with no admissible labelling on the way since
% Start; Mine is Excluded and the candidates tried before on the way. Excluded are the candidates tried before in the branches
% above, whose own branches found every admissible labelling above them that has them IN: none is IN below.
% sccSeen(Start, In) marks a labelling of the search from Start as reached
admissibleAbove(Closure, Scc, Excluded, Start, Branch, Admissible, Mine) :-
    undecidedCandidates(Scc, Branch, Excluded, Candidates),
    Candidates = [_ | _],
    findall(I, store::argumentLabelByIdIn(Branch, in, I), Ins),
    candidate(Candidates, Excluded, IdX, Tried),
    utils::sortDescending([IdX | Ins], Seed),
    \+ cacheCheck(sccSeen(Start, Seed)),
    cacheAssert(sccSeen(Start, Seed)),
    contextBranch(Branch, NewBranch),
    forceIn(Closure, IdX),
    force(Closure, Scc, Tried),
    store::labelledIds(in, Reached),
    utils::sortDescending(Reached, Labelled),
    unexplored(Start, Seed, Labelled),
    (
        labelSets::admissibleWithin(Scc)
    ->
        Admissible = NewBranch,
        Mine = Tried
    ;
        admissibleAbove(Closure, Scc, Tried, Start, NewBranch, Admissible, Mine)
    ).

% candidate(+Candidates, +Excluded, -Id, -Mine): each candidate in turn, with those before it excluded
candidate([Id | _], Excluded, Id, Excluded).
candidate([Tried | Candidates], Excluded, Id, Mine) :-
    candidate(Candidates, [Tried | Excluded], Id, Mine).

% force(+Closure, +Scc, +Excluded): the defenders the IN arguments of Scc leave a single choice for forced IN, one
% at a time, each followed by its step; fails when one is left with none (no admissible labelling is above the
% state), or when an excluded argument is IN (what is above the state was found already)
% (a component of one argument has nothing to force: its only candidate is an argument attacking itself)
force(_, [_], _) :- !.
force(Closure, Scc, Excluded) :-
    forcing(Scc, Excluded, Next),
    forced(Next, Closure, Scc, Excluded).

forced(none, _, _, _).
forced(defender(D), Closure, Scc, Excluded) :-
    forceIn(Closure, D),
    force(Closure, Scc, Excluded).

% A labelling only depends on the set of arguments forced IN and on what is excluded, and a state explored with
% some excluded misses only labellings found from where they were tried: the labelling reached by forcing In is
% marked as reached, whatever the order

unexplored(_, Seed, Seed) :- !.
unexplored(Start, _, Labelled) :-
    \+ cacheCheck(sccSeen(Start, Labelled)),
    cacheAssert(sccSeen(Start, Labelled)).
