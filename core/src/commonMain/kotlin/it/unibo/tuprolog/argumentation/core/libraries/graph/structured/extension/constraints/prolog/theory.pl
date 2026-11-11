%! stepScope(-Scope)
%  own: the step goes through the attacks or the arguments it changes each alone, which the solvers of a session
%  building the graph together divide (see pipeline::stepScope/2).
stepScope(own).

%! modifyArgumentationGraph
%  The instances the arguments rest on (see supportInstances/0), and the pieces of the open arguments (see pieces/0),
%  with their attacks. An argument split into pieces stays as it is, its attacks and the attacks on it too: it holds for
%  all its values.
modifyArgumentationGraph :-
    supportInstances,
    pieces,
    findall(Id, contextCheck(addedInstance(Id)), Ids),
    contextRetract(addedInstance(_)),
    contextRetract(pieceOf(_, _, _)),
    attackBuilder::buildAttacksOf(Ids).

%! supportInstances
%  The instances the saved arguments rest on: a support used for fewer values than it has is saved by the builder
%  as bound by the premise it supports, the argument it is an instance of saved alone.
supportInstances :-
    findall(S, (
        store::support(S, A),
        store::argumentId(S, Id),
        \+ store::inGraph(Id)
    ), Instances),
    findall(_, (member(Instance, Instances), realize(Instance)), _).

%! realize(+Instance)
%  Instance, a saved argument with bindings, saved unless it is (an instance for fewer values), with the supports
%  of the argument bound alike: instances in turn where the values flow into them.
realize(Instance) :-
    store::argumentId(Instance, Id),
    store::inGraph(Id), !.
realize(Instance) :-
    general(Instance, General), !,
    store::supportersOf(General, Supporters),
    store::body(General, Body),
    store::body(Instance, Bound),
    store::conclusion(Instance, Conclusion),
    store::saveInstance(Conclusion, Instance),
    store::argumentId(Instance, Id),
    contextAssert(addedInstance(Id)),
    findall(_, (member(Sub, Supporters), realizeSupport(Sub, Body, Bound, Instance)), _).
realize(_).

% The saved argument Instance is an instance of
general(Instance, General) :-
    store::conclusion(Instance, Conclusion),
    store::argumentUnifiable(Conclusion, General),
    utils::subsumes(General, Instance).

% Sub, supporting the premise of Body that Bound, the body of Instance, binds: its instance for that binding
realizeSupport(Sub, Body, Bound, Instance) :-
    store::claim(Sub, Claim),
    premiseAt(Body, Claim, Bound, Premise),
    copy_term(Sub, Instantiated),
    store::claim(Instantiated, Premise),
    realize(Instantiated),
    store::saveSupport(Instantiated, Instance).

premiseAt([P|_], Claim, [B|_], B) :- \+ \+ P = Claim, !.
premiseAt([_|Ps], Claim, [_|Bs], B) :- premiseAt(Ps, Claim, Bs, B).

%==============================================================================
% PIECES. An open argument (with variables) stands for each of the values its constraints allow. Where an attack
% holds for some of them only, the argument is split: into the piece the attack holds for, the argument with the
% constraints of the attacker added, and the free pieces, the argument with them negated one at a time (A, not b1;
% A, b1, not b2; ...), the empty ones left out (decided by the simplex, see satisfiable/1). The regions cutting an
% argument are those of its attackers (their pieces, once split) and of the pieces of its sub-arguments, mapped on
% its variables: each piece rests on the piece of each sub-argument holding for its values, so that the attacks on
% a sub-argument reach only the pieces they hold for. An attack whose region needs variables of the attacker that
% the attacked argument does not have (as -p(f(Y)) on p(X)) cuts nothing: the argument is attacked as a whole.
% The cuts are worked out until no argument splits further, then the pieces saved, sub-arguments first.
%
% A region is a list of atoms over the variables of a copy of the argument: eq(X, T) (a binding of the attack or of
% a sub-argument), lin(C) (a linear constraint), dif(X, T) (the negation of a binding).
%==============================================================================

%! pieces
%  The open arguments split into pieces where attacks hold for some of their values.
pieces :-
    store::arguments(Arguments),
    openArguments(Arguments, Open),
    Open \== [], !,
    ordered(Open, Ordered),
    splitRounds(Ordered, 1),
    savePieces(Ordered),
    contextRetract(cells(_, _)),
    contextRetract(depthOf(_, _)).
pieces.

openArguments([], []).
openArguments([A|As], [A|Open]) :- \+ ground(A), !, openArguments(As, Open).
openArguments([_|As], Open) :- openArguments(As, Open).

% The arguments, each after its sub-arguments
ordered(Arguments, Ordered) :-
    findall(D-A, (member(A, Arguments), depth(A, D)), Pairs),
    findall(D, member(D-_, Pairs), Depths),
    maxDepth(Depths, 0, Max),
    findall(A, (between(1, Max, D), member(D-A, Pairs)), Ordered).

depth(A, D) :-
    store::argumentId(A, Id),
    (contextCheck(depthOf(Id, D)) ->
        true
    ;
        store::supportersOf(A, Supporters),
        findall(DS, (member(S, Supporters), depth(S, DS)), Depths),
        maxDepth(Depths, 0, Max),
        D is Max + 1,
        contextAssert(depthOf(Id, D))
    ).

maxDepth([], Max, Max).
maxDepth([D|Ds], Acc, Max) :- (D > Acc -> maxDepth(Ds, D, Max) ; maxDepth(Ds, Acc, Max)).

% Rounds of splitting until no argument splits further (the regions are made of the atoms of the theory, finitely
% many: the bound is a guard)
splitRounds(Ordered, Round) :-
    findall(changed, (member(A, Ordered), resplit(A)), Changes),
    (Changes == [] -> true ; Round >= 32 -> true ; Next is Round + 1, splitRounds(Ordered, Next)).

% A split again by the regions known now: succeeds if its pieces are more
resplit(A) :-
    store::argumentId(A, Id),
    cuts(A, Cuts),
    copy_term(A, W),
    splitAll(Cuts, W, [cell([], [])], Cells),
    length(Cells, N),
    N > 1,
    (contextCheck(cells(Id, _-Old)) -> length(Old, M) ; M = 1),
    N > M,
    contextRetract(cells(Id, _)),
    contextAssert(cells(Id, W-Cells)).

% The regions cutting A, tagged with what they come from: attacker(IdB, K) for the piece K of an attacker (0 for an
% attacker not split), sub(IdS, K) for the piece K of a sub-argument
cuts(A, Cuts) :-
    findall(Cut, attackerCut(A, Cut), FromAttackers),
    findall(Cut, subCut(A, Cut), FromSubs),
    append(FromAttackers, FromSubs, Cuts).

attackerCut(A, cut(attacker(IdB, K), QA-Atoms)) :-
    store::argumentId(A, IdA),
    attackSearch::possibleAttackers(IdA, A, [Head]),
    store::argumentUnifiable([Head], B),
    store::argumentId(B, IdB),
    cellOf(B, IdB, K, CB, BAtoms),
    copy_term(A, QA),
    utils::termVariables(QA, Qs),
    copy_term(A, CA),
    utils::termVariables(CA, Vars),
    applied(BAtoms, BConstraints),
    conflict::attacker(CB),
    attackRelation::attacks(_, CB, CA),
    constraintsOf(CB, Own),
    append(Own, BConstraints, Constraints),
    conjuncts(Constraints, Region),
    constraintsOf(CA, Domain),
    append(Domain, Region, Together),
    satisfiable(Together),
    regionAtoms(Vars, Region, Qs, Atoms).

subCut(A, cut(sub(IdS, K), QA-Atoms)) :-
    store::supportersOf(A, Supporters),
    member(S, Supporters),
    store::argumentId(S, IdS),
    contextCheck(cells(IdS, _)),
    cellOf(S, IdS, K, CS, SAtoms),
    copy_term(A, QA),
    utils::termVariables(QA, Qs),
    copy_term(A, CA),
    utils::termVariables(CA, Vars),
    applied(SAtoms, SConstraints),
    store::claim(CS, Claim),
    store::body(CA, Body),
    once((member(P, Body), store::premiseKind(P, statement), P = Claim)),
    constraintsOf(CA, Domain),
    append(Domain, SConstraints, Together),
    satisfiable(Together),
    regionAtoms(Vars, SConstraints, Qs, Atoms).

% The pieces of an argument (K from 1, a copy CB of it bound by the bindings of the piece, the other atoms of the piece),
% or the argument itself (K = 0) if it is not split
cellOf(B, IdB, K, CB, Atoms) :-
    contextCheck(cells(IdB, W-Cells)), !,
    numbered(Cells, 1, K, cell(CellAtoms, _)),
    copy_term(W-CellAtoms, CB-Atoms).
cellOf(B, _, 0, CB, []) :-
    copy_term(B, CB).

% numbered(+List, +From, -K, -Element): Element is the element K of List, counted from From
numbered([X|_], K, K, X).
numbered([_|Xs], At, K, X) :- Next is At + 1, numbered(Xs, Next, K, X).

% The atoms of a region applied to the copy they are on: the bindings made, the constraints left as a list of
% constraints (dif(X, T) and the linear ones)
applied([], []).
applied([eq(X, T)|Atoms], Cs) :- !, X = T, applied(Atoms, Cs).
applied([lin(C)|Atoms], [C|Cs]) :- !, applied(Atoms, Cs).
applied([dif(X, T)|Atoms], [dif(X, T)|Cs]) :- applied(Atoms, Cs).

% regionAtoms(+Vars, +Constraints, +Qs, -Atoms): the region over Qs (the variables of a copy of the argument) of the
% values Vars (the variables of another copy, bound by an attack or by a sub-argument) take, with Constraints:
% fails if it needs other variables, or a constraint the module does not decide
regionAtoms(Vars, Constraints, Qs, Atoms) :-
    aliases(Vars, Qs, []),
    bindings(Vars, Qs, Bindings),
    constraintAtoms(Constraints, ConstraintAtoms),
    append(Bindings, ConstraintAtoms, Atoms),
    utils::termVariables(Atoms, Used),
    within(Used, Qs).

% The values that are variables, first met, become the variables of the region
aliases([], [], _).
aliases([V|Vs], [Q|Qs], Seen) :-
    (var(V), \+ (member(S, Seen), S == V) -> V = Q, aliases(Vs, Qs, [Q|Seen]) ; aliases(Vs, Qs, Seen)).

% The others are bindings: X = T for a value T that is a term, or a variable met before
bindings([], [], []).
bindings([V|Vs], [Q|Qs], Bindings) :-
    (V == Q -> Bindings = Rest ; Bindings = [eq(Q, V)|Rest]),
    bindings(Vs, Qs, Rest).

constraintAtoms([], []).
constraintAtoms([C|Cs], Atoms) :- ground(C), !, constraintAtoms(Cs, Atoms).
constraintAtoms([dif(X, T)|Cs], [dif(X, T)|Atoms]) :- !, constraintAtoms(Cs, Atoms).
constraintAtoms([C|Cs], [lin(C)|Atoms]) :- linearConstraint(C), constraintAtoms(Cs, Atoms).

within([], _).
within([V|Vs], Qs) :- member(Q, Qs), Q == V, !, within(Vs, Qs).

% splitAll(+Cuts, +W, +Cells, -Split): Cells, the pieces of W (cell(Atoms, Tags)), split by each of Cuts in turn
splitAll([], _, Cells, Cells).
splitAll([cut(Tag, QA-Atoms)|Cuts], W, Cells, Split) :-
    copy_term(QA-Atoms, W2-Region),
    W2 = W,
    refineAll(Cells, W, Tag, Region, Next),
    splitAll(Cuts, W, Next, Split).

refineAll([], _, _, _, []).
refineAll([Cell|Cells], W, Tag, Region, Split) :-
    refined(Cell, W, Tag, Region, Pieces),
    refineAll(Cells, W, Tag, Region, Rest),
    append(Pieces, Rest, Split).

% A piece the region misses stays as it is; one inside it is tagged with it; one it cuts gives the piece inside it and
% the free ones
refined(cell(Atoms, Tags), W, Tag, Region, Pieces) :-
    added(Region, Atoms, Inside),
    (nonEmpty(W, Inside) ->
        free(Region, [], W, Atoms, Tags, Free),
        Pieces = [cell(Inside, [Tag|Tags])|Free]
    ;
        Pieces = [cell(Atoms, Tags)]
    ).

% added(+Region, +Atoms, -All): Atoms with those of Region they do not have already (the same term)
added([], Atoms, Atoms).
added([R|Rs], Atoms, All) :- member(A, Atoms), A == R, !, added(Rs, Atoms, All).
added([R|Rs], Atoms, All) :- append(Atoms, [R], More), added(Rs, More, All).

% free(+Region, +Before, +W, +Atoms, +Tags, -Free): the free pieces, each atom of Region negated after those before it
free([], _, _, _, _, []).
free([Atom|Atoms], Before, W, CellAtoms, Tags, Free) :-
    negations(Atom, W, CellAtoms, Negations),
    freeParts(Negations, Before, W, CellAtoms, Tags, Parts),
    append(Before, [Atom], Next),
    free(Atoms, Next, W, CellAtoms, Tags, Rest),
    append(Parts, Rest, Free).

freeParts([], _, _, _, _, []).
freeParts([Negated|Negations], Before, W, CellAtoms, Tags, Parts) :-
    append(CellAtoms, Before, Kept),
    append(Kept, [Negated], Atoms),
    (nonEmpty(W, Atoms) -> Parts = [cell(Atoms, Tags)|Rest] ; Parts = Rest),
    freeParts(Negations, Before, W, CellAtoms, Tags, Rest).

% The negations of an atom, its alternatives: a binding to a number, by the numbers below and above it when the
% variable is a number (it has a linear constraint), by dif otherwise
negations(eq(X, T), W, Atoms, [lin(X < T), lin(X > T)]) :- number(T), numeric(X, W, Atoms), !.
negations(eq(X, T), _, _, [dif(X, T)]) :- !.
negations(dif(X, T), _, _, [eq(X, T)]) :- !.
negations(lin(C), _, _, Negations) :- negatedLinear(C, Negations).

negatedLinear(A < B, [lin(A >= B)]).
negatedLinear(A =< B, [lin(A > B)]).
negatedLinear(A > B, [lin(A =< B)]).
negatedLinear(A >= B, [lin(A < B)]).
negatedLinear(A =:= B, [lin(A < B), lin(A > B)]).
negatedLinear(A = B, [lin(A < B), lin(A > B)]).

numeric(X, W, Atoms) :-
    constraintsOf(W, Cs),
    conjuncts(Cs, Own),
    linearAtoms(Atoms, Others),
    append(Own, Others, All),
    member(C, All),
    linearConstraint(C),
    utils::termVariables(C, Vs),
    member(V, Vs),
    V == X, !.

% The linear constraints of the atoms, nothing bound
linearAtoms([], []).
linearAtoms([lin(C)|Atoms], [C|Cs]) :- !, linearAtoms(Atoms, Cs).
linearAtoms([_|Atoms], Cs) :- linearAtoms(Atoms, Cs).

% The piece of W with Atoms holds for some value
nonEmpty(W, Atoms) :-
    \+ \+ (
        copy_term(W-Atoms, C-Copied),
        applied(Copied, Constraints),
        constraintsOf(C, Own),
        append(Own, Constraints, All),
        satisfiable(All)
    ).

% The pieces saved, sub-arguments first: each the argument with the bindings of its atoms and their constraints added
% to its body, resting on the pieces of its sub-arguments its values are in
savePieces([]).
savePieces([A|As]) :-
    store::argumentId(A, Id),
    (contextCheck(cells(Id, W-Cells)) -> savePieces(Cells, 1, Id, A, W) ; true),
    savePieces(As).

savePieces([], _, _, _, _).
savePieces([cell(Atoms, Tags)|Cells], K, Id, A, W) :-
    savePiece(Atoms, Tags, K, Id, A, W),
    Next is K + 1,
    savePieces(Cells, Next, Id, A, W).

savePiece(Atoms, Tags, K, Id, A, W) :-
    copy_term(W-Atoms, P0-Copied),
    applied(Copied, Constraints),
    constraintPremises(Constraints, Premises),
    store::argumentRules(P0, Rules),
    store::topRule(P0, Top),
    store::conclusion(P0, Conclusion),
    store::body(P0, Body),
    store::argumentInfo(P0, Info),
    append(Body, Premises, PieceBody),
    store::newArgument(Rules, Top, Conclusion, PieceBody, Info, Piece),
    store::argumentId(Piece, PieceId),
    (store::inGraph(PieceId) -> true ; store::saveInstance(Conclusion, Piece), contextAssert(addedInstance(PieceId))),
    contextAssert(pieceOf(Id, K, PieceId)),
    store::supportersOf(A, Supporters),
    findall(_, (member(S, Supporters), pieceSupport(S, Tags, Support), store::saveSupport(Support, Piece)), _).

constraintPremises([], []).
constraintPremises([C|Cs], ['{}'(C)|Ps]) :- constraintPremises(Cs, Ps).

% The support of a piece: the piece of the sub-argument its values are in, the sub-argument if not split
pieceSupport(S, Tags, Support) :-
    store::argumentId(S, IdS),
    member(sub(IdS, K), Tags),
    contextCheck(pieceOf(IdS, K, IdPiece)), !,
    store::argumentById(IdPiece, Support).
pieceSupport(S, _, S).
