%! solve(+Arguments, +Attacks, -In, -Out, -Und)
%  The labelling of the abstract framework of Arguments (atoms) and Attacks ((A, B) pairs), by argumentLabellingMode: the arguments IN, OUT and UND.
solve(Arguments, Attacks, I, O, U) :-
    solve(Arguments, Attacks),
    store::claimLabelling(I, O, U).

%! solve(+Arguments, +Attacks)
%  The labelling of the abstract framework of Arguments and Attacks, in the context: the framework saved as the
%  graph builder of abstract theories saves it (see abstractGraph::save/2), and labelled as it is (the graph
%  extensions change the graphs built from rules).
solve(Arguments, Attacks) :-
    context_reset,
    phases::checkProfile,
    findall(att(X, Y), member((X, Y), Attacks), Atts),
    abstractGraph::save(Arguments, Atts),
    phases::buildArgumentLabelling.
