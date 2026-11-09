%! applies
%  The step applies to a theory whose arguments hold for sets of values (see instances/0), whatever the flags state.
applies :- instances.

%! stepScope(-Scope)
%  own: the step goes through the attacks or the arguments it changes each alone, which the solvers of a session
%  building the graph together divide (see phases::stepScope/2).
stepScope(own).

%! modifyArgumentationGraph
%  The instances the arguments rest on (see supportInstances/0) and those a conflict binds (see
%  buildConflictInstances/0), with their attacks.
modifyArgumentationGraph :-
    supportInstances,
    buildConflictInstances,
    findall(Id, context_check(addedInstance(Id)), Ids),
    context_retract(addedInstance(_)),
    attackBuilder::buildAttacksOf(Ids).

%! supportInstances
%  The instances the saved arguments rest on: a support used for fewer values than it has is saved by the builder
%  as bound by the premise it supports, the argument it is an instance of saved alone.
supportInstances :-
    findall(S, (
        store::support(S, A),
        store::argumentId(S, Id),
        \+ store::inGraph(Id)
    ), Instances),
    findall(_, (member(Instance, Instances), realize(Instance)), _).

%! buildConflictInstances
%  The instances where a conflict binds the variables of an argument.
buildConflictInstances :-
    store::arguments(Arguments),
    findall(_, (member(A, Arguments), conflictInstance(A)), _).

% The instances of A and of its possible attackers for the values a conflict between them binds
conflictInstance(A) :-
    store::claim(A, ClaimA),
    store::argumentId(A, IdA),
    attackCandidates::possibleAttackers(IdA, A, [Head]),
    store::argumentUnifiable([Head], B),
    store::claim(B, ClaimB),
    \+ (ground(ClaimA), ground(ClaimB)),
    copy_term(B-A, CB-CA),
    attackRelation::attacks(_, CB, CA),
    compatible(CB, CA),
    realize(CA),
    realize(CB),
    fail.
conflictInstance(_).

%! realize(+Instance)
%  Instance, a saved argument with bindings, saved unless it is (an instance for fewer values), with the supports
%  of the argument bound alike: instances in turn where the values flow into them.
realize(Instance) :-
    store::argumentId(Instance, Id),
    store::inGraph(Id), !.
realize(Instance) :-
    general(Instance, General), !,
    store::supportersOf(General, Supporters),
    store::body(General, Body),
    store::body(Instance, Bound),
    store::conclusion(Instance, Conclusion),
    store::saveInstance(Conclusion, Instance),
    store::argumentId(Instance, Id),
    context_assert(addedInstance(Id)),
    findall(_, (member(Sub, Supporters), realizeSupport(Sub, Body, Bound, Instance)), _).
realize(_).

% The saved argument Instance is an instance of
general(Instance, General) :-
    store::conclusion(Instance, Conclusion),
    store::argumentUnifiable(Conclusion, General),
    utils::subsumes(General, Instance).

% Sub, supporting the premise of Body that Bound, the body of Instance, binds: its instance for that binding
realizeSupport(Sub, Body, Bound, Instance) :-
    store::claim(Sub, Claim),
    premiseAt(Body, Claim, Bound, Premise),
    copy_term(Sub, Instantiated),
    store::claim(Instantiated, Premise),
    realize(Instantiated),
    store::saveSupport(Instantiated, Instance).

premiseAt([P|_], Claim, [B|_], B) :- \+ \+ P = Claim, !.
premiseAt([_|Ps], Claim, [_|Bs], B) :- premiseAt(Ps, Claim, Bs, B).
