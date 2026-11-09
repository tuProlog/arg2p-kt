%==============================================================================
% BP COMPLETE LABELLING [ICLP]
%==============================================================================

%! locality(-Locality)
%  Query mode: not known to be directional (the burdens of proof order the arguments along the attacks), the
%  label of an argument is taken to depend on the arguments connected to it, in either direction
locality(connected).

%! argumentLabelling
%  The labelling, in the active context.
argumentLabelling :-
    bp_grounded::reifyBurdenOfProofs,
    store::arguments(Arguments),
    completeBpLabelling(go, Arguments).

completeBpLabelling(stop, _).
completeBpLabelling(go, Arguments) :-
    bp_grounded_partial::partialBpLabelling(Arguments), !,
    store::labelledArguments(und, NewArgs),
    store::unlabelArguments(und),
    bp_grounded::groundedLabelling(NewArgs),
    store::labelledArguments(und, LeftArguments),
    stopCondition(Stop, Arguments, LeftArguments),
    completeBpLabelling(Stop, LeftArguments).

stopCondition(stop, Args, NewArgs) :-
    utils::sortDescending(Args, SortedArgs),
    utils::sortDescending(NewArgs, SortedArgs), !.
stopCondition(go, _, _) :- store::unlabelArguments(und).
