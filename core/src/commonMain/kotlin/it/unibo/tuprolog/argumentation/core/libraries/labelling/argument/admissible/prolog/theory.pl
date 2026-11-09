%! locality(-Locality)
%  Query mode: the label of an argument only depends on the arguments it depends on (directionality)
locality(directional).

%! argumentLabelling
%  Each admissible labelling in turn, left as the active context: the search of the SCC-recursive semantics,
%  its labellings not closed (what the IN arguments defend need not be IN), every admissible one kept.
argumentLabelling :-
    scc::label(admissible, all, none).
