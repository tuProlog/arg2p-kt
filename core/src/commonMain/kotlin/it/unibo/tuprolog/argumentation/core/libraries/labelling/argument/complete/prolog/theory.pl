% Complete labellings are SCC-recursive: built component by component, keeping the admissible local ones

%! locality(-Locality)
%  Query mode: the label of an argument only depends on the arguments it depends on (directionality)
locality(directional).

%! argumentLabelling
%  Each complete labelling in turn, left as the active context.
argumentLabelling :-
    scc::label(admissible, all).
