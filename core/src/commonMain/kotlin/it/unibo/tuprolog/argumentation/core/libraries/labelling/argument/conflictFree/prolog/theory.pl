%! locality(-Locality)
%  Query mode: the label of an argument only depends on the arguments it depends on (directionality)
locality(directional).

%! argumentLabelling
%  Each conflict-free labelling in turn, left as the active context.
argumentLabelling :-
    contextActive(Branch),
    labelSets::candidates(Args, Forced),
    conflictFreeSubset(Args, Forced, Set),
    labelSets::labelSet(Branch, Set).
