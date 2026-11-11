%! preferenceMode
%  This graph extension weighs the attacks by the preferences (see pipeline::preferenceMode/1).
preferenceMode.

%! stepScope(-Scope)
%  own: the step goes through the attacks or the arguments it changes each alone, which the solvers of a session
%  building the graph together divide (see pipeline::stepScope/2).
stepScope(own).

%! modifyArgumentationGraph
%  As defeasiblePref, the attack arguments attacked by an argument for all the preferences invalidating them
%  together (see defeasiblePref::modifyArgumentationGraph/1).
modifyArgumentationGraph :-
    defeasiblePref::modifyArgumentationGraph(all).

%! queryDependencies(+Argument, -Statement)
%  Query mode: the same arguments matter as for defeasiblePref
queryDependencies(Argument, S) :- defeasiblePref::queryDependencies(Argument, S).
%! queryDependants(+Argument, -Statement)
queryDependants(Argument, S) :- defeasiblePref::queryDependants(Argument, S).
