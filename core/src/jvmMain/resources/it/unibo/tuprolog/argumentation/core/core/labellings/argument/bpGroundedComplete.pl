%==============================================================================
% BP COMPLETE LABELLING [ICLP]
%==============================================================================

argumentLabelling :-
    bp_grounded::reifyBurdenOfProofs,
    findall(X, context_check(argument(X)), Arguments),
    completeBpLabelling(go, Arguments).

completeBpLabelling(stop, _).
completeBpLabelling(go, Arguments) :-
    bp_grounded_partial::partialBpLabelling(Arguments), !,
    findall(X, (context_check(und(X)), context_retract(und(X))), NewArgs),
    grounded::groundedLabelling(NewArgs),
    findall(X, context_check(und(X)), LeftArguments),
    stopCondition(Stop, Arguments, LeftArguments),
    completeBpLabelling(Stop, LeftArguments).

stopCondition(stop, Args, NewArgs) :-
    utils::sort(Args, SortedArgs),
    utils::sort(NewArgs, SortedArgs), !.
stopCondition(go, _, _) :- context_retract(und(_)).
