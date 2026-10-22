% Complete labellings are SCC-recursive: built component by component, keeping the admissible local ones

% Query mode: the label of an argument only depends on the arguments it depends on (directionality)
locality(directional).

argumentLabelling :-
    scc::label(admissible, all).
