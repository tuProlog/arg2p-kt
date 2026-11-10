%! computeStatementAcceptance(+Goal, -Result)
%  The labels of the statements for Goal: in(S), out(S) and und(S) terms.
computeStatementAcceptance(Goal, Res) :-
    setting(answerMode, enumerate), !,
    computeStatementAcceptance(Goal),
    mineResults(Goal, Res).
computeStatementAcceptance(Goal, Res) :-
    computeStatementAcceptance(Goal, In, Out, Und),
    findall(R, (member(S, In), R = in(S) ; member(S, Out), R = out(S) ; member(S, Und), R = und(S)), Res).

%! computeStatementAcceptance(+Goal, -In, -Out, -Und)
%  The statements for Goal labelled IN, OUT and UND, as answerMode states: in each labelling, one solution for
%  each (enumerate), or across them all (credulous, skeptical, see acceptance/5).
computeStatementAcceptance(Goal, YesResult, NoResult, UndResult) :-
    setting(answerMode, enumerate), !,
    computeStatementAcceptance(Goal),
    mineResults(Goal, YesResult, NoResult, UndResult).
computeStatementAcceptance(Goal, YesResult, NoResult, UndResult) :-
    setting(answerMode, Mode),
    findall([Yes, No, Und], (computeStatementAcceptance(Goal), mineResults(Goal, Yes, No, Und)), Labellings),
    acceptance(Mode, Labellings, YesResult, NoResult, UndResult).

%! accepted(?Goal)
%  Goal is a statement labelled IN, as answerMode states (see computeStatementAcceptance/4).
accepted(Goal) :-
    setting(answerMode, enumerate), !,
    computeStatementAcceptance(Goal),
    answer(in, Goal, Answer),
    (Answer = [Goal, '{}'(_)] -> true ; Goal = Answer).
accepted(Goal) :-
    computeStatementAcceptance(Goal, In, _, _),
    member(Goal, In).

% acceptance(+Mode, +Labellings, -In, -Out, -Und): the statements across Labellings, the [In, Out, Und] of each
% labelling. Credulous: IN those IN in some labelling, OUT those OUT in every one. Skeptical: IN those IN in
% every labelling, OUT those OUT in some. UND the others: none is in two of the three.
acceptance(Mode, Labellings, In, Out, Und) :-
    findall(S, (member(Labelling, Labellings), member(Statements, Labelling), member(S, Statements)), Found),
    distinctStatements(Found, [], All),
    findall(S, (member(S, All), acceptedAs(Mode, in, S, Labellings)), In0),
    findall(S, (member(S, All), \+ acceptedAs(Mode, in, S, Labellings), acceptedAs(Mode, out, S, Labellings)), Out0),
    findall(S, (member(S, All), \+ acceptedAs(Mode, in, S, Labellings), \+ acceptedAs(Mode, out, S, Labellings)), Und0),
    utils::sortDescending(In0, In),
    utils::sortDescending(Out0, Out),
    utils::sortDescending(Und0, Und).

acceptedAs(credulous, in, S, Labellings) :- member([Yes, _, _], Labellings), memberStatement(S, Yes), !.
acceptedAs(credulous, out, S, Labellings) :- \+ (member([_, No, _], Labellings), \+ memberStatement(S, No)).
acceptedAs(skeptical, in, S, Labellings) :- \+ (member([Yes, _, _], Labellings), \+ memberStatement(S, Yes)).
acceptedAs(skeptical, out, S, Labellings) :- member([_, No, _], Labellings), memberStatement(S, No), !.

distinctStatements([], Distinct, Distinct).
distinctStatements([S|Ss], Seen, Distinct) :-
    (memberStatement(S, Seen) -> distinctStatements(Ss, Seen, Distinct) ; distinctStatements(Ss, [S|Seen], Distinct)).

% S is one of the statements, the same up to the names of its variables (each labelling has its own)
memberStatement(S, [T|_]) :- sameStatement(S, T), !.
memberStatement(S, [_|Ts]) :- memberStatement(S, Ts).

sameStatement(S, T) :- ground(S), !, S == T.
sameStatement(S, T) :- utils::subsumes(S, T), utils::subsumes(T, S).

%! computeStatementAcceptance(+Goal)
%  The labels of the statements for Goal, in the context, on the part of the graph they depend on (see pipeline::evaluate/1).
computeStatementAcceptance(Goal) :-
    pipeline:::evaluate(Goal).

mineResults(Goal, YesResult, NoResult, UndResult) :-
    answers(Goal, Answers),
    findall(A, member(in-A, Answers), In),
    findall(A, member(out-A, Answers), Out),
    findall(A, member(und-A, Answers), Und),
    utils::sortDescending(In, YesResult),
    utils::sortDescending(Out, NoResult),
    utils::sortDescending(Und, UndResult).

mineResults(Goal, Result) :-
    answers(Goal, Answers),
    findall(in(A), member(in-A, Answers), In),
    findall(out(A), member(out-A, Answers), Out),
    findall(und(A), member(und-A, Answers), Und),
    utils::appendLists([In, Out, Und], Result).

%! answers(+Goal, -Answers)
%  The statements labelled answering Goal, as Label-Answer pairs (see answer/3).
answers(Goal, Answers) :-
    parser::hook(admitted/3, Extension), !,
    findall(Label-Statement, store::statementLabel(Label, Statement), Labelled0),
    findall(Label-Answer, (member(Label-Statement, Labelled0), Extension::admitted(Statement, Goal, Answer)), Labelled),
    bestAnswers(Labelled, [], Answers).
answers(Goal, Answers) :-
    findall(Label-Goal, store::statementLabel(Label, [Goal]), Answers).

%! answer(?Label, ?Goal, -Answer)
%  Answer, labelled Label, answers Goal: a statement unifying with it. With an extension whose arguments hold for sets
%  of values (defining admitted/3, see parser::hook/2), Goal is answered by the statements it admits: Answer is Goal,
%  bound as they bind it, or [Goal, {C}] when constraints C are still open on it; several statements answering it
%  alike (the same up to the names of their variables), it takes the first label of in, out and und any of them has,
%  as a statement takes those of its arguments.
answer(Label, Goal, Answer) :-
    answers(Goal, Answers),
    member(Label-Answer, Answers).

%! answering(+Answer, -Argument)
%  Argument stands for Answer, one of answer/3: its claim is Answer (the same up to the names of its variables); with
%  an extension defining admitted/3, its statement admits the values of Answer.
answering(Answer, Argument) :-
    parser::hook(admitted/3, Extension), !,
    (Answer = [Goal, '{}'(_)] -> true ; Goal = Answer),
    store::argumentUnifiable([Goal], Argument),
    store::statement(Argument, Statement),
    \+ \+ (Extension::admitted(Statement, Goal, Admitted), sameStatement(Admitted, Answer)).
answering(Answer, Argument) :-
    store::argumentUnifiable([Answer], Argument),
    store::claim(Argument, Claim),
    sameStatement(Claim, Answer).

% The answers with the best label among those answering alike (in, then out, then und)
bestAnswers([], Answers, Answers).
bestAnswers([Label-Answer|Labelled], Seen, Answers) :-
    (alike(Answer, Seen, Known) ->
        better(Label, Known, Seen, Next)
    ;
        Next = [Label-Answer|Seen]
    ),
    bestAnswers(Labelled, Next, Answers).

alike(Answer, [Label-Known|_], Label-Known) :- sameStatement(Answer, Known), !.
alike(Answer, [_|Seen], Known) :- alike(Answer, Seen, Known).

better(Label, KnownLabel-Known, Seen, Next) :-
    (precedes(Label, KnownLabel) -> replaced(Seen, KnownLabel-Known, Label-Known, Next) ; Next = Seen).

precedes(in, out).
precedes(in, und).
precedes(out, und).

replaced([X|Xs], Old, New, [New|Xs]) :- X == Old, !.
replaced([X|Xs], Old, New, [X|Ys]) :- replaced(Xs, Old, New, Ys).
