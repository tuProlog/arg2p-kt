argumentGroundedLabelling([Arguments, Attacks, _], [IN, OUT, UND]) :-
    groundedLabelling(Arguments, Attacks, [], [], Arguments, _, IN, OUT, UND), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

groundedLabelling(Arguments, Attacks, IN, OUT, UND, ResultAttacks, ResultIN, ResultOUT, ResultUND) :-
    member(A, UND), 
    allAttacksOUT(Attacks, A, OUT),
    subtract(UND, [A], NewUND),
    % expandPreferenceSet(A, Arguments, Attacks, NewAttacks),
    groundedLabelling(Arguments, Attacks, [A|IN], OUT, NewUND, ResultAttacks, ResultIN, ResultOUT, ResultUND).
groundedLabelling(Arguments, Attacks, IN, OUT, UND, ResultAttacks, ResultIN, ResultOUT, ResultUND) :-
    member(A, UND), 
    oneAttackIN(Attacks, A, IN),
    subtract(UND, [A], NewUND),
    groundedLabelling(Arguments, Attacks, IN, [A|OUT], NewUND, ResultAttacks, ResultIN, ResultOUT, ResultUND).
groundedLabelling(_, Attacks, IN, OUT, UND, Attacks, IN, OUT, UND).

/*
    If an attack exists, it should come from an OUT argument
*/
allAttacksOUT(Attacks, A, OUT) :-
    \+ ( member((_, B, A), Attacks), \+ ( member(B, OUT))).

/*
    Find an attack, if exists, from an IN argument, then ends
*/
oneAttackIN(Attacks, A, IN) :-
    member((_, B, A), Attacks),
    member(B, IN), !.

/*
    If A attacks an IN argument, then A is OUT
*/
oneAttackIN(Attacks, A, IN) :-
    member((_, A, B), Attacks),
    member(B, IN), !.

% /*
%     If the argument conclusion is a sup indication compute the new attack set
% */
% expandPreferenceSet([_, _, [sup(RuleOne, RuleTwo)]], Arguments, Attacks, NewAttacks) :-
%     assert(temp_sup(RuleOne, RuleTwo)),
%     assert(sup(RuleOne, RuleTwo)),
%     findall(A, computeInvalidAttack(RuleOne, RuleTwo, Arguments, Attacks, A), InvalidAttacks),
%     subtract(Attacks, InvalidAttacks, NewAttacks).

% expandPreferenceSet(_, _, Attacks, Attacks).

% /*
%     Find one of the involved attacks and verifies if it is still relevant considering the new preference
% */
% computeInvalidAttack(RuleOne, RuleTwo, Arguments, Attacks, (T, [R1, TR1, C1],[R2, TR2, C2])) :-
%     member((T, [R1, TR1, C1],[R2, TR2, C2]), Attacks),
%     eligible(RuleOne, RuleTwo, R1, R2),
%     invalid(T, [R1, TR1, C1],[R2, TR2, C2]).

% eligible(RuleOne, RuleTwo, R1, R2) :- 
%     (member(RuleOne, R1);member(RuleTwo, R1);member(RuleOne, R2);member(RuleTwo, R2)), !.

% invalid(rebut, A, B) :- superiorArgument(B, A), !.
% invalid(undermine, A, B) :- superiorArgument(B, A), !.

% /*
%     Given a list of arguments and a target argument, returns the list of derived arguments
% */
% findDerivedArguments([IdTwo, _, _], Arguments, Derived) :-
%     findall([Id, T, C], (member([Id, T, C], Arguments), subset(IdTwo, Id), IdTwo \== Id), Derived).

% subset([], _).
% subset([H|T], List) :- member(H, List), subset(T,List).

% /*
%     Defeasible preference addition
% */
% conflict([sup(X, Y)],  [sup(Y, X)]).

cleanTempSup :-
    findall(_,(temp_sup(X, Y), retract(sup(X, Y))), _),
    retractall(temp_sup(_, _)).
