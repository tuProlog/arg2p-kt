%! locality(-Locality)
%  Query mode: stable labellings depend on the whole graph (an odd cycle anywhere leaves none)
locality(whole_graph).

%! argumentLabelling
%  Stable labellings are SCC-recursive: built component by component, leaving no argument UND
argumentLabelling :-
    scc::label(decided, all).
