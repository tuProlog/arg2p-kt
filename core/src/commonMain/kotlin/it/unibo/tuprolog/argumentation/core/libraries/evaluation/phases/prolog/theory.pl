%! dispatched(+Module, +Predicate)
%  Module defines Predicate (Name/Arity), one the phases call on the modules the flags select: an existence
%  error otherwise, rather than the failure of the whole evaluation an unknown predicate gives.
dispatched(Module, Name/Arity) :-
    (module_defines(Module, Name/Arity) ->
        true
    ;
        throw(error(existence_error(procedure, ':'(Module, Name/Arity)), 'the module does not define the predicate the phases call on it'))
    ).

%! builder(-Builder)
%  The module building the graph: that of the flag graphBuilder, ruleGraph if no setting names one.
builder(X) :- setting(graphBuilder, X), !.
builder(ruleGraph).

%! checkProfile
%  The theory is written in the languages of its profile (see parser::checkLanguages), and its flags go
%  together: one preference mode at most (see preferenceMode/1). Checked once for the theory (see
%  utils::oncePerTheory/2): a theory failing it raises its error at every solve.
checkProfile :- utils::oncePerTheory(profile, phases::profileChecked).

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
    step(Extension),
    module_defines(Extension, preferenceMode/0).

%! modifyGraph
%  The graph changed by the steps the flags select (see active/1), in the order of steps/1 whatever the order the
%  flags state them in: first those adding the arguments derived from the others, each with their attacks (the
%  conflicts as arguments of metaConflicts), then those changing the
%  attacks (rebutRestriction, the preference mode, bp). Any other graph extension (a module of the user) comes
%  last, in the order stated.
%  With the solvers of a session changing it together (see GraphSharing), each step once every solver has merged
%  what the others did before it, those changing the attacks on each argument alone (see stepScope/2) going
%  through the attacks on the arguments of each solver.
modifyGraph :-
    plan(Steps, Others, _),
    applySteps(Steps),
    modifyGraph(Others),
    store::graph_step(all).

%! querySteps(-Steps)
%  The steps changing the graph (see modifyGraph/0) that relate statements in query mode, defining
%  queryDependencies/2 and queryDependants/2 (see queryExploration::extend/1), in their order.
querySteps(Steps) :- plan(_, _, Steps).

%! plan(-Steps, -Others, -QuerySteps)
%  What modifyGraph/0 and querySteps/1 go through, worked out once for the theory (see utils::oncePerTheory/2), as
%  whether a step applies is a function of the theory and of the flags (see active/1): Steps those of steps/1, as
%  step(Step, Scope, Applies) (see stepScope/2), Applies yes or no; Others the graph extensions of the user (see
%  others/1); QuerySteps the applying steps relating statements in query mode, in their order.
plan(Steps, Others, QuerySteps) :-
    utils::oncePerTheory(plan, phases::readPlan),
    cache_check(stepsPlan(Steps, Others, QuerySteps)).

readPlan :-
    steps(All),
    planned(All, Steps),
    others(Others),
    findall(Step, (member(step(Step, _, yes), Steps), queryStep(Step)), Known),
    findall(Step, (member(Step, Others), active(Step), queryStep(Step)), Theirs),
    append(Known, Theirs, QuerySteps),
    cache_assert(stepsPlan(Steps, Others, QuerySteps)).

planned([], []).
% (a step not applying runs nothing until the next step: its scope is of no use, and its module is not loaded for it)
planned([Step|Steps], [step(Step, Scope, Applies)|Planned]) :-
    (active(Step) -> Applies = yes, stepScope(Step, Scope) ; Applies = no, Scope = all),
    planned(Steps, Planned).

queryStep(Step) :-
    module_defines(Step, queryDependencies/2),
    module_defines(Step, queryDependants/2).

%! steps(-Steps)
%  The modules changing the graph, in the order they apply.
steps([constraints, conflictArguments, rebutRestriction, standardPref, defeasiblePref, defeasibleAllPref, abaPref, bp]).

%! step(?Step)
%  Step is one of steps/1.
step(Step) :- steps(Steps), member(Step, Steps).

%! active(+Step)
%  Step applies: when it says so, defining applies/0 (a step applying as the theory is, not by a flag: a function of
%  the theory and of the flags, asked once for the theory, see plan/3), when it is a graph extension otherwise; any
%  other graph extension does (see others/1).
active(Step) :- module_defines(Step, applies/0), !, Step::applies.
active(Step) :- setting(graphExtension, Step).

% The graph extensions of the user, the modules other than those of steps/1, as stated
others(Others) :- findall(Y, (setting(graphExtension, Y), \+ step(Y)), Others).

applySteps([]).
applySteps([step(Step, Scope, Applies)|Steps]) :-
    store::graph_step(Scope),
    (Applies == yes -> Step:::modifyArgumentationGraph ; true),
    applySteps(Steps).

% stepScope(+Step, -Scope): as the step declares it (stepScope/1): own for a step going through the attacks
% (attackById/4, directAttacks/1 of the module store), or the arguments it changes (its enumerations of them), each
% alone, which the solvers of a session building the graph together divide by the arguments they saved, all of them
% looked up; all otherwise, each solver changing the whole graph alike
stepScope(Step, Scope) :- module_defines(Step, stepScope/1), !, Step::stepScope(Scope).
stepScope(_, all).

modifyGraph([]).
modifyGraph([X|Ext]) :-
    store::graph_step(all),
    dispatched(X, modifyArgumentationGraph/0),
    X:::modifyArgumentationGraph,
    modifyGraph(Ext).

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
