%! locality(-Locality)
%  Query mode: the label of an argument only depends on the arguments it depends on (directionality)
locality(directional).

%! argumentLabelling
%  The ideal labelling has IN the maximal admissible subset of the intersection of the preferred extensions
argumentLabelling :-
    context_active(Branch),
    labelSets::maximalAdmissibleCommonOf(preferred, Branch).
