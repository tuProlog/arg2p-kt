%! evaluate
%  The graph of the theory, whole, changed by the steps the flags select, and its argument and statement labellings, in the
%  context: one solution for each labelling.
evaluate :- evaluated(whole, whole_graph).

%! evaluate(+Goal)
%  As evaluate/0, on the part of the graph the labels of the statements for Goal depend on, as locality/1 goes: with
%  queryMode, and a semantics and a builder allowing it; the whole graph otherwise.
evaluate(Goal) :-
    locality(Locality),
    evaluated(Goal, Locality).

% evaluated(+Query, +Locality): the graph for Query as Locality goes (see graph/2), changed and labelled
evaluated(Query, Locality) :-
    graph(Query, Locality),
    modifyGraph,
    buildArgumentLabelling,
    buildStatementLabelling.

%! statementLabels(-In, -Out, -Und)
%  As evaluate/0, with the labels of the statements, one solution for each labelling.
statementLabels(In, Out, Und) :-
    evaluate,
    store::statementLabelling(In, Out, Und).

%! graph(+Query, +Locality)
%  The graph, by the builder of the profile (graphBuilder), once the theory is checked against it, not changed by the steps
%  yet: whole for whole_graph (buildArgumentationGraph/0), otherwise the part the labels of the arguments for Query depend on,
%  as Locality goes (buildQueryGraph/3, given the steps that apply, see steps/1). With the
%  solvers of a session building it together, the graph of all of them (see store::graph_begin/1, store::graph_settle/0).
graph(Query, Locality) :-
    checkProfile,
    builder(Builder),
    (Locality == whole_graph ->
        store::graph_begin(whole),
        dispatched(Builder, buildArgumentationGraph/0),
        Builder:::buildArgumentationGraph
    ;
        store::graph_begin(Query),
        steps(Steps),
        dispatched(Builder, buildQueryGraph/3),
        Builder:::buildQueryGraph(Query, Locality, Steps)
    ),
    store::graph_settle.

%! locality(-Locality)
%  The part of the graph the labels of the arguments for a query depend on: with queryMode, as the semantics declares it
%  (locality/1), the arguments they depend on (directional) or those connected to them in either direction (connected);
%  otherwise the whole graph (whole_graph: queryMode off, a semantics that does not declare it, a builder giving no part of
%  the graph, defining no buildQueryGraph/3, as for an abstract theory).
locality(whole_graph) :- \+ setting(queryMode), !.
locality(Locality) :-
    builder(Builder),
    (module_defines(Builder, buildQueryGraph/3) -> semanticsLocality(Locality) ; Locality = whole_graph).

semanticsLocality(Locality) :-
    setting(argumentLabellingMode, Semantics),
    (module_defines(Semantics, locality/1) -> Semantics::locality(Locality) ; Locality = whole_graph).

%! dispatched(+Module, +Predicate)
%  Module defines Predicate (Name/Arity), one the pipeline calls on the modules the flags select: an existence
%  error otherwise, rather than the failure of the whole evaluation an unknown predicate gives.
dispatched(Module, Name/Arity) :-
    (module_defines(Module, Name/Arity) ->
        true
    ;
        throw(error(existence_error(procedure, ':'(Module, Name/Arity)), 'the module does not define the predicate the pipeline calls on it'))
    ).

%! builder(-Builder)
%  The module building the graph: that of the flag graphBuilder, ruleGraph if no setting names one.
builder(X) :- setting(graphBuilder, X), !.
builder(ruleGraph).

%! checkProfile
%  The theory is written in the languages of its profile (see parser::checkLanguages), and its flags go
%  together: one preference mode at most (see preferenceMode/1). Checked once for the theory (see
%  utils::oncePerTheory/2): a theory failing it raises its error at every solve.
checkProfile :- utils::oncePerTheory(profile, pipeline::profileChecked).

profileChecked :-
    parser::checkLanguages,
    checkExtensions.

checkExtensions :-
    findall(P, (setting(graphExtension, P), preferenceMode(P)), Modes),
    (Modes = [_, _|_] ->
        throw(error(domain_error(one_preference_mode, Modes), 'graphExtension: one preference mode at most, the graph extensions weighing the attacks by the preferences'))
    ;
        true
    ).

%! preferenceMode(?Extension)
%  Extension weighs the attacks by the preferences: a theory has one at most.
preferenceMode(Extension) :-
    order(Order),
    member(Extension, Order),
    module_defines(Extension, preferenceMode/0).

%! modifyGraph
%  The graph changed by the steps the flags select (see plan/1), in their order whatever the order the flags state them in:
%  first those adding the arguments derived from the others, each with their attacks (the conflicts as arguments of
%  metaConflicts), then those changing the attacks (rebutRestriction, the preference mode, bp), then any other graph
%  extension (a module of the user), in the order stated.
%  With the solvers of a session changing it together (see GraphSharing), each step once every solver has merged
%  what the others did before it, those changing the attacks on each argument alone (see stepScope/2) going
%  through the attacks on the arguments of each solver.
modifyGraph :-
    plan(Plan),
    applySteps(Plan),
    store::graph_step(all).

%! steps(-Steps)
%  The steps changing the graph that apply (see modifyGraph/0), in their order.
steps(Steps) :-
    plan(Plan),
    findall(Step, member(step(Step, _, yes), Plan), Steps).

%! plan(-Plan)
%  The steps modifyGraph/0 goes through, as step(Step, Scope, Applies) (see stepScope/2), Applies yes or no: those of
%  order/1, then the other graph extensions stated, in the order stated. Worked out once for the theory (see
%  utils::oncePerTheory/2), as whether a step applies is a function of the theory and of the flags (see active/1). A step
%  that does not apply is gone through all the same: the solvers of a session, each with its part of the theory, on which a
%  step may apply or not, meet at every step (see store::graph_step/1).
plan(Plan) :-
    utils::oncePerTheory(plan, pipeline::readPlan),
    cache_check(stepsPlan(Plan)).

readPlan :-
    order(Order),
    findall(Other, (setting(graphExtension, Other), \+ member(Other, Order)), Others),
    append(Order, Others, All),
    planned(All, Plan),
    cache_assert(stepsPlan(Plan)).

planned([], []).
% (a step not applying runs nothing until the next step: its scope is of no use, and its module is not loaded for it)
planned([Step|Steps], [step(Step, Scope, Applies)|Planned]) :-
    (active(Step) -> Applies = yes, stepScope(Step, Scope) ; Applies = no, Scope = all),
    planned(Steps, Planned).

%! order(-Steps)
%  The modules changing the graph arg2p has, in the order they apply.
order([constraints, conflictArguments, rebutRestriction, standardPref, defeasiblePref, defeasibleAllPref, abaPref, bp]).

%! active(+Step)
%  Step applies: it is one of the extensions of the theory (see parser::extension/1): a graph extension stated, one
%  required by another extension (as temporal requires constraints), or one on by its flag (as conflictArguments by
%  metaConflicts). No module is loaded for it.
active(Step) :- parser::extension(Step), !.

applySteps([]).
applySteps([step(Step, Scope, Applies)|Steps]) :-
    store::graph_step(Scope),
    (Applies == yes -> dispatched(Step, modifyArgumentationGraph/0), Step:::modifyArgumentationGraph ; true),
    applySteps(Steps).

% stepScope(+Step, -Scope): as the step declares it (stepScope/1): own for a step going through the attacks
% (attackById/4, directAttacks/1 of the module store), or the arguments it changes (its enumerations of them), each
% alone, which the solvers of a session building the graph together divide by the arguments they saved, all of them
% looked up; all otherwise, each solver changing the whole graph alike
stepScope(Step, Scope) :- module_defines(Step, stepScope/1), !, Step::stepScope(Scope).
stepScope(_, all).

%! buildArgumentLabelling
%  The labelling of the arguments, by the semantics of argumentLabellingMode.
buildArgumentLabelling :-
    setting(argumentLabellingMode, X),
    dispatched(X, argumentLabelling/0),
    X:::argumentLabelling.

%! buildStatementLabelling
%  The labelling of the statements, by statementLabellingMode.
buildStatementLabelling :-
    setting(statementLabellingMode, X),
    dispatched(X, statementLabelling/0),
    X:::statementLabelling.
