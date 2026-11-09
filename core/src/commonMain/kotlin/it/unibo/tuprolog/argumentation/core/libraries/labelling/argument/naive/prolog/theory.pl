%! locality(-Locality)
%  Query mode: not directional, but the labellings of unconnected parts of the graph are independent: the label
%  of an argument depends on the arguments connected to it, in either direction
locality(connected).

%! argumentLabelling
%  Each naive labelling in turn, left as the active context.
argumentLabelling :-
    context_active(Branch),
    labelSets::candidates(Args, Forced),
    store::labelledIds(in, In),
    naiveSubset(Args, Forced, In, Set),
    labelSets::labelSet(Branch, Set).
