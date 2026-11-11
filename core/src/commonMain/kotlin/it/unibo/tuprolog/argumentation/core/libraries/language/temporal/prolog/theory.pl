%! requires(-Extension)
%  The arguments of its axioms conclude open statements, holding for the times of a period: the arguments for sets of
%  values of the graph extension constraints (see parser::extension/1).
requires(constraints).

%! rules(-Rules)
%  The axioms, as the parser reads the defeasible rules of a theory ([Id, Premises, Conclusion]).
rules([
    % an event starts or ends U at its time; starting V ends what V conflicts with
    [tStart(E, U, T), (happens(E, T), initiates(E, U)), started(U, T)],
    [tStop(E, U, T), (happens(E, T), terminates(E, U)), ended(U, T)],
    [tEnds(E, U, T), (happens(E, T), initiates(E, V), prolog(conflict::between([V], [U]))), ended(U, T)],
    % the periods after a start, before an end the theory states (a start of what conflicts with U tells nothing
    % of what held before it), and from a start to a later end
    [tAfter(U, T1), (started(U, T1), {T > T1}), holdsAt(U, T)],
    [tBefore(E, U, T2), (happens(E, T2), terminates(E, U), {T =< T2}), holdsAt(U, T)],
    [tPeriod(U, T1, T2), (started(U, T1), ended(U, T2), prolog(T1 < T2), {T > T1, T =< T2}), holdsAt(U, T)],
    % an end in between: not one period
    [tBroken(U, T1, T2, T), (started(U, T1), ended(U, T2), ended(U, T), prolog((T1 < T, T < T2))), -tPeriod(U, T1, T2)],
    % an end after a start closes the period after it, a start before an end the period before it
    [tCloses(U, T1, T2), (started(U, T1), ended(U, T2), prolog(T1 < T2)), -tAfter(U, T1)],
    [tOpens(E, U, T1, T2), (started(U, T1), happens(E, T2), terminates(E, U), prolog(T1 < T2)), -tBefore(E, U, T2)],
    % a start while an earlier start still holds (no end in between) is no new start
    [tAlready(E, U, T0, T), (started(U, T0), happens(E, T), initiates(E, U), prolog(T0 < T)), -tStart(E, U, T)],
    [tStill(E, U, T0, T, Te), (started(U, T0), ended(U, Te), happens(E, T), initiates(E, U), prolog((T0 < Te, Te =< T))), -tAlready(E, U, T0, T)]
]).
