%! locality(-Locality)
%  Query mode: not directional, but the labellings of unconnected parts of the graph are independent: the label
%  of an argument depends on the arguments connected to it, in either direction
locality(connected).

%! argumentLabelling
%  Stage labellings are the naive ones with a minimal set of UND arguments
argumentLabelling :-
    contextActive(Branch),
    labelSets::minimalUndOf(naive, Branch).
