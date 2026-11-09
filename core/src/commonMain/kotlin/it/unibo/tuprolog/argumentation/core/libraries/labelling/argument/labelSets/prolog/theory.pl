%! candidates(-Args, -Forced)
%  The arguments to label, and those among them already IN (kept IN)
candidates(Args, Forced) :-
    store::argumentIds(Args),
    findall(A, (member(A, Args), store::argumentLabelById(in, A)), Forced).

%! labelSet(+Branch, +Set)
%  In a new branch of Branch, Set IN, the arguments it attacks OUT, the rest UND;
%  the branch is released on backtracking
labelSet(Branch, Set) :-
    newLabelling(Branch, Set),
    context_active(Labelling),
    (
        true
    ;
        context_checkout(Branch),
        context_drop(Labelling),
        fail
    ).

%! minimalUndOf(+Semantics, +Branch)
%  The labellings of Semantics with a minimal set of UND arguments (semi-stable for preferred, stage for naive),
%  each labelled in a branch of Branch.
minimalUndOf(Semantics, Branch) :-
    findall(Und-In, (
        Semantics:::argumentLabelling,
        store::labelledIds(und, Und),
        store::labelledIds(in, In)
    ), Labellings),
    utils::minimalBySet(Labellings, Minimal),
    member(In, Minimal),
    labelSet(Branch, In).

%! maximalAdmissibleCommonOf(+Semantics, +Branch)
%  The labelling with IN the maximal admissible subset of the intersection of the labellings of Semantics
%  (ideal for preferred, eager for semistable), in a branch of Branch.
maximalAdmissibleCommonOf(Semantics, Branch) :-
    findall(In, (
        Semantics:::argumentLabelling,
        store::labelledIds(in, In)
    ), Labellings),
    utils::intersectionOf(Labellings, Common),
    maximalAdmissibleSubset(Common, Maximal),
    labelSet(Branch, Maximal).
