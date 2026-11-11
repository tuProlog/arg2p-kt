%! locality(-Locality)
%  Query mode: the label of an argument only depends on the arguments it depends on (directionality)
locality(directional).

%! argumentLabelling
%  Preferred labellings are SCC-recursive: built component by component, keeping the admissible local
%  labellings with a maximal set of IN arguments
argumentLabelling :-
    scc::label(admissible, maximal).
