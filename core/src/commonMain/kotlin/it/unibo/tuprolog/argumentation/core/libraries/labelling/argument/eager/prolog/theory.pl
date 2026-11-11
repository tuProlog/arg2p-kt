%! locality(-Locality)
%  Query mode: not directional, but the labellings of unconnected parts of the graph are independent: the label
%  of an argument depends on the arguments connected to it, in either direction
locality(connected).

%! argumentLabelling
%  The eager labelling has IN the maximal admissible subset of the intersection of the semi-stable extensions
argumentLabelling :-
    contextActive(Branch),
    labelSets::maximalAdmissibleCommonOf(semiStable, Branch).
