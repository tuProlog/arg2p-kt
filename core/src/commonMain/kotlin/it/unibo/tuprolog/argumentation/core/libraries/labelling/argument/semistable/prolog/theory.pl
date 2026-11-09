%! locality(-Locality)
%  Query mode: not directional, but the labellings of unconnected parts of the graph are independent: the label
%  of an argument depends on the arguments connected to it, in either direction
locality(connected).

%! argumentLabelling
%  Semi-stable labellings are the preferred ones with a minimal set of UND arguments
%  (an admissible set with a maximal range is a maximal admissible set)
argumentLabelling :-
    context_active(Branch),
    labelSets::minimalUndOf(preferred, Branch).
