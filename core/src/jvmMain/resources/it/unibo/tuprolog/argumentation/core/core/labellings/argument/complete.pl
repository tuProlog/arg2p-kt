% Complete labellings are SCC-recursive: built component by component, keeping the admissible local ones

argumentLabelling :-
    scc::label(admissible, all).
