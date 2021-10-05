%==============================================================================
% BP COMPLETE LABELLING [ICLP]
%==============================================================================

argumentLabelling(Rules, [Arguments, Attacks, Supports], [In, Out, Und]) :-
    bp_grounded::reifyBurdenOfProofs(Rules, Arguments, Bps),
    utils::sort(Arguments, ArgumentsS),
    hbpComplete(go, Bps, Attacks, Supports, [], [], ArgumentsS, In, Out, Und).

hbpComplete(stop, _, _, _, In, Out, Und, In, Out, Und).
hbpComplete(go, Bps, Attacks, Supports, In, Out, Und, ResultIn, ResultOut, ResultUnd) :-
    bp_grounded_partial::partialHBPLabelling(Bps, Supports, Und, In, Out, [], BaseIn, BaseOut, BaseUnd),
    grounded::groundedLabelling(Attacks, BaseIn, BaseOut, BaseUnd, CompleteIn, CompleteOut, CompleteUnd),
    utils::sort(CompleteIn, CompleteInS),
    utils::sort(CompleteOut, CompleteOutS),
    utils::sort(CompleteUnd, CompleteUndS),
    stopCondition(Stop, In, CompleteInS, Out, CompleteOutS, Und, CompleteUndS),
    hbpComplete(Stop, Bps, Attacks, Supports, CompleteInS, CompleteOutS, CompleteUndS, ResultIn, ResultOut, ResultUnd).

stopCondition(stop, In, In, Out, Out, Und, Und) :- !.
stopCondition(go, _, _, _, _, _, _).
