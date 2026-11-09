%! applies
%  The step applies when the conflicts are arguments (see conflict::argued/0): no flag names it.
applies :- conflict::argued.

%! stepScope(-Scope)
%  own: the step goes through the attacks or the arguments it changes each alone, which the solvers of a session
%  building the graph together divide (see phases::stepScope/2).
stepScope(own).

%! modifyArgumentationGraph
%  With metaConflicts, the copies (see buildCopies/0) and their attacks, the only ones.
modifyArgumentationGraph :-
    buildCopies,
    findall(Id, context_check(conflictCopy(Id)), Ids),
    context_retract(conflictCopy(_)),
    attackBuilder::buildAttacksOf(Ids).

%! buildCopies
%  With metaConflicts, the copies of the saved arguments for each argument for a conflict on their conclusion.
buildCopies :-
    findall(_, buildCopy, _).

% (each saved argument, then the conflicts on its conclusion)
buildCopy :-
    store::argumentFor(Conclusion, Argument),
    \+ conflict::carried(Argument, _),
    conflictArgument(Conclusion, ConflictArgument),
    store::claim(ConflictArgument, Conflict),
    store::argumentId(ConflictArgument, ConflictId),
    store::argumentRules(Argument, Rules),
    store::withArgumentRules(Argument, [mc(ConflictId, Conflict)|Rules], Copy),
    store::saveArgument(Conclusion, Copy),
    store::argumentId(Copy, Id),
    context_assert(conflictCopy(Id)),
    store::saveSupport(ConflictArgument, Copy),
    store::copySupports(Argument, Id),
    fail.
buildCopy.

% An argument for a conflict of the statement Conclusion with another one
conflictArgument([A], ConflictArgument) :-
    store::argumentFor([conflict(A, _, _)], ConflictArgument).
conflictArgument([A], ConflictArgument) :-
    store::argumentFor([conflict(A, _)], ConflictArgument).

%==============================================================================
% QUERY MODE
%==============================================================================

%! queryDependencies(+Argument, -Statement)
%  The conflicts are themselves conclusions: Argument depends on the arguments for Statement, which make others
%  conflict with it, and, for an argument for a conflict, on those for the two sides it puts in conflict, whatever
%  the locality: the copies of those arguments are what it makes attack.
queryDependencies(Argument, Statement) :-
    store::conclusion(Argument, Conclusion),
    conflict::statement(_, Conclusion, Statement).
queryDependencies(Argument, Statement) :-
    store::topRule(Argument, Rule),
    Rule \== none,
    conflict::statement(_, [Rule], Statement).
queryDependencies(Argument, Statement) :-
    store::claim(Argument, Conflict),
    conflict::statement([A], [B], Conflict),
    (Statement = A ; Statement = B).

%! queryDependants(+Argument, -Statement)
%  The arguments for Statement depend on Argument: those its conclusion makes conflict with others.
queryDependants(Argument, Statement) :-
    store::conclusion(Argument, Conclusion),
    conflict::statement(Conclusion, _, Statement).
