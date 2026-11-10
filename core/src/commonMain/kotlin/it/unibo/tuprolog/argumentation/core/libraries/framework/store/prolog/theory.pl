%==========================================================================
% The terms of the graph
%==========================================================================

% An argument is the list [Rules, TopRule, Conclusion, Body, Info], read and built through the predicates below rather
% than by the positions of its parts (see ArgumentTerm); a statement is [Claim]. Each relates an argument to a part,
% so it works both ways: given an argument it reads the part, given a part it builds the template of the arguments
% with it, to look them up in the context (argumentFor([sup(X, Y)], A)).

%! newArgument(?Rules, ?TopRule, ?Conclusion, ?Body, ?Info, ?Argument)
%  Argument is made of its parts.
newArgument(Rules, TopRule, Conclusion, Body, Info, [Rules, TopRule, Conclusion, Body, Info]).

%! argumentRules(?Argument, ?Rules)
%  Rules are the identifiers of the rules and premises of Argument.
argumentRules([Rules, _, _, _, _], Rules).

%! withArgumentRules(?Argument, ?Rules, ?Changed)
%  Changed is Argument with the rules Rules.
withArgumentRules([_, TopRule, Conclusion, Body, Info], Rules, [Rules, TopRule, Conclusion, Body, Info]).

%! topRule(?Argument, ?TopRule)
%  TopRule is the last rule of Argument, none for an argument made of a premise alone.
topRule([_, TopRule, _, _, _], TopRule).

%! conclusion(?Argument, ?Conclusion)
%  Conclusion is the statement Argument concludes, [Claim].
conclusion([_, _, Conclusion, _, _], Conclusion).

%! claim(?Argument, ?Claim)
%  Claim is the term Argument concludes.
claim([_, _, [Claim], _, _], Claim).

%! body(?Argument, ?Body)
%  Body are the premises of the top rule of Argument.
body([_, _, _, Body, _], Body).

%! argumentInfo(?Argument, ?Info)
%  Info is what the preferences weigh of Argument: [LastDefeasibleRules, DefeasibleRules, DefeasiblePremises].
argumentInfo([_, _, _, _, Info], Info).

%! lastDefeasibleRules(?Argument, ?Rules)
%  Rules are the last defeasible rules of Argument.
lastDefeasibleRules([_, _, _, _, [Rules, _, _]], Rules).

%! infoParts(?Info, ?LastDefeasibleRules, ?DefeasibleRules, ?DefeasiblePremises)
%  Info is made of its parts.
infoParts([LastDefeasibleRules, DefeasibleRules, DefeasiblePremises], LastDefeasibleRules, DefeasibleRules, DefeasiblePremises).

%! isPremise(+Argument)
%  Argument is made of a premise alone.
isPremise([_, TopRule, _, _, _]) :- TopRule == none.

%! accumulated(+Support, +Parts, -Accumulated)
%  Parts, the rules, last defeasible rules, defeasible rules and defeasible premises of some arguments (a list of
%  lists each, as rulesAndInfo/5 takes them), with those of Support first in each.
accumulated(Support, [All, LastDef, Def, Prem], [[Rules|All], [SupportLastDef|LastDef], [SupportDef|Def], [SupportPrem|Prem]]) :-
    argumentRules(Support, Rules),
    argumentInfo(Support, Info),
    infoParts(Info, SupportLastDef, SupportDef, SupportPrem).

%! premiseInfo(+Id, +Strictness, -Info)
%  The info of an argument made of the premise Id alone, strict or defeasible: none of its premises defeasible if it is
%  strict.
premiseInfo(_, strict, Info) :- !, infoParts(Info, [], [], []).
premiseInfo(Id, _, Info) :- infoParts(Info, [], [], [Id]).

%! rulesAndInfo(+RuleId, +Strict, +Supports, -Rules, -Info)
%  The rules and the info of the argument built from the rule RuleId (strict if Strict is strict) on sub-arguments
%  with the rules, last defeasible rules, defeasible rules and defeasible premises in Supports (see RulesAndInfo);
%  native.

%! isDefeasible(+Argument)
%  Argument has defeasible rules or premises.
isDefeasible([_, _, _, _, Info]) :- Info \== [[], [], []].

%! defeasibleTopRule(+Argument, ?Rule)
%  Rule is the top rule of Argument and its last defeasible rule.
defeasibleTopRule(Argument, Rule) :-
    topRule(Argument, Rule),
    lastDefeasibleRules(Argument, [Rule]).

% The labels as facts of the context (see LabelTerm): an argument labelled IN, OUT or UND is in(Argument), out(Argument)
% or und(Argument), by its identifier inId(Id), outId(Id) or undId(Id) (the native labellings), a statement
% statIn(Statement), statOut(Statement) or statUnd(Statement). A label is in, out or und.

%! labelFact(?Label, ?Argument, ?Fact)
%  Fact labels Argument with Label.
labelFact(in, Argument, in(Argument)).
labelFact(out, Argument, out(Argument)).
labelFact(und, Argument, und(Argument)).

%! labelIdFact(?Label, ?Id, ?Fact)
%  Fact labels the argument with identifier Id with Label.
labelIdFact(in, Id, inId(Id)).
labelIdFact(out, Id, outId(Id)).
labelIdFact(und, Id, undId(Id)).

%! statementLabelFact(?Label, ?Statement, ?Fact)
%  Fact labels Statement with Label.
statementLabelFact(in, Statement, statIn(Statement)).
statementLabelFact(out, Statement, statOut(Statement)).
statementLabelFact(und, Statement, statUnd(Statement)).

%--------------------------------------------------------------------------
% Arguments
%--------------------------------------------------------------------------

%! saveArgument(+Conclusion, +Argument)
%  Argument, whose conclusion is Conclusion, is saved, unless it already is.
saveArgument(Conclusion, Argument) :-
    context_absent(argument(Argument)),
    conclusion(Argument, Conclusion),
    context_id(argument(Argument), Id),
    record(Argument, Id).

%! saveArgument(+Tag, +Conclusion, +Argument)
%  As saveArgument/2, Conclusion noted as new under Tag (see newConclusion/2): the rounds of the graph builder.
saveArgument(Tag, Conclusion, Argument) :-
    saveArgument(Conclusion, Argument),
    context_assert(newConc(Tag, Conclusion)).

%! saveInstance(+Conclusion, +Argument)
%  As saveArgument/2, unless an argument with the identifier of Argument is saved: Argument is saved even when an
%  argument it is an instance of is.
saveInstance(Conclusion, Argument) :-
    context_id(argument(Argument), Id),
    context_absent(arg(Id)),
    conclusion(Argument, Conclusion),
    record(Argument, Id).

record(Argument, Id) :-
    context_assert_id(Id),
    context_assert(arg(Id)),
    argumentInfo(Argument, Info),
    context_id(info(Info), InfoId),
    context_assert(argInfo(Id, InfoId)).

%! argument(?Argument)
%  Argument is saved.
argument(Argument) :-
    context_check(argument(Argument)).

%! arguments(-Arguments)
%  Arguments are the saved arguments (see argumentById/2 for a graph built by several solvers).
arguments(Arguments) :-
    graph_own(Ids), !,
    findall(Argument, (member(Id, Ids), context_term(Id, argument(Argument))), Arguments).
arguments(Arguments) :-
    findall(Argument, context_check(argument(Argument)), Arguments).

%! argumentFor(?Conclusion, ?Argument)
%  Argument is saved for Conclusion. Going through the arguments (Conclusion unbound): see argumentById/2.
argumentFor(Conclusion, Argument) :-
    nonvar(Conclusion), !,
    context_by(conclusion, Conclusion, argument(Argument)).
argumentFor(Conclusion, Argument) :-
    graph_own(Ids), !,
    member(Id, Ids),
    context_term(Id, argument(Argument)),
    conclusion(Argument, Conclusion).
argumentFor(Conclusion, Argument) :-
    context_check(argument(Argument)),
    conclusion(Argument, Conclusion).

%! argumentUnifiable(+Conclusion, -Argument)
%  Argument is saved for a conclusion unifying with Conclusion, which it is not bound to.
argumentUnifiable(Conclusion, Argument) :-
    context_matching(conclusion, Conclusion, argument(Argument)).

%! argumentById(?Id, ?Argument)
%  Argument is saved with the identifier Id. While the solvers of a session divide the work on a graph (building
%  it, a step dividing it: see GraphSharing), going through the arguments (nothing bound) gives those this solver
%  saved, as every enumeration of the store does (arguments/1, argumentFor/2, support/2, attackById/4,
%  directAttacks/1); a lookup (a key bound) goes through all of them.
argumentById(Id, Argument) :-
    var(Id),
    graph_own(Ids), !,
    member(Id, Ids),
    context_term(Id, argument(Argument)).
argumentById(Id, Argument) :-
    context_check(arg(Id)),
    context_term(Id, argument(Argument)).

%! inGraph(?Id)
%  The argument with identifier Id is in the graph the labellings label (arg/1, see leaveOutArguments/0): no argument
%  read.
inGraph(Id) :-
    context_check(arg(Id)).

%! graphSize(-Count)
%  Count arguments are in the graph the labellings label.
graphSize(Count) :-
    context_count(arg(_), all, Count).

%! inGraph(+Branch, ?Id)
%  The argument with identifier Id is in the graph the labellings label in the context branch Branch.
inGraph(Branch, Id) :-
    context_check(Branch, arg(Id)).

%! argumentId(+Argument, -Id)
%  Id is the identifier of Argument (see context_id/2).
argumentId(Argument, Id) :-
    context_id(argument(Argument), Id).

%! argumentIds(-Ids)
%  Ids are the identifiers of the saved arguments.
argumentIds(Ids) :-
    findall(Id, context_check(arg(Id)), Ids).

%! leaveOutArguments
%  The saved arguments are left out of the graph the labellings label (by their identifiers), until taken in again
%  (takeInArgument/1); they stay saved.
leaveOutArguments :-
    context_retract(arg(_)).

%! takeInArgument(+Id)
%  The argument with identifier Id is in the graph the labellings label again.
takeInArgument(Id) :-
    context_assert(arg(Id)).

%! argumentInfoId(?Id, ?InfoId)
%  The argument with identifier Id has the info with identifier InfoId.
argumentInfoId(Id, InfoId) :-
    context_check(argInfo(Id, InfoId)).

%! infoById(+InfoId, -Info)
%  Info is the info with identifier InfoId.
infoById(InfoId, Info) :-
    context_term(InfoId, info(Info)).

%! newConclusion(+Tag, ?Conclusion)
%  An argument for Conclusion was saved under Tag.
%  With the solvers of a session building the graph together (see GraphSharing): what this one saved passed on, and
%  what the others did taken, as new under Tag; with nothing new, until another does or none can.
newConclusion(Tag, Conclusion) :-
    (context_check(newConc(Tag, _)) -> graph_share(Tag, false) ; graph_share(Tag, true)),
    context_check(newConc(Tag, Conclusion)).

%! forgetNewConclusions(?Tag)
%  The conclusions saved under Tag are forgotten (not their arguments).
forgetNewConclusions(Tag) :-
    context_retract(newConc(Tag, _)).

%--------------------------------------------------------------------------
% Supports
%--------------------------------------------------------------------------

%! saveSupport(+Supporter, +Supported)
%  Supporter, a sub-argument of Supported, is saved as supporting it, unless it already is: by their identifiers,
%  as the support of an instance of Supported by one of Supporter (for fewer values) is another one. Saved already
%  it still succeeds: a rule may use the same argument for two of its premises.
saveSupport(Supporter, Supported) :-
    context_id(argument(Supporter), IdSupporter),
    context_id(argument(Supported), IdSupported),
    (context_absent(supportId(IdSupporter, IdSupported)) -> context_assert(supportId(IdSupporter, IdSupported)) ; true).

%! copySupports(+Original, +IdCopy)
%  The supports of Original also support the argument with identifier IdCopy.
copySupports(Original, IdCopy) :-
    argumentId(Original, IdOriginal),
    findall(S, context_check(supportId(S, IdOriginal)), Supporters),
    findall(_, (member(S, Supporters), (context_absent(supportId(S, IdCopy)) -> context_assert(supportId(S, IdCopy)) ; true)), _).

%! saveSupporters(+Argument, +Supporters)
%  Each of Supporters saved as a support of Argument (see saveSupport/2).
saveSupporters(Argument, Supporters) :-
    findall(_, (member(Supporter, Supporters), saveSupport(Supporter, Argument)), _).

%! support(?Supporter, ?Supported)
%  Supporter supports Supported.
support(Supporter, Supported) :-
    nonvar(Supported), !,
    context_stored_id(argument(Supported), IdSupported),
    context_check(supportId(IdSupporter, IdSupported)),
    supportOf(IdSupporter, Supporter, IdSupported, Supported).
support(Supporter, Supported) :-
    nonvar(Supporter), !,
    context_stored_id(argument(Supporter), IdSupporter),
    context_check(supportId(IdSupporter, IdSupported)),
    supportOf(IdSupporter, Supporter, IdSupported, Supported).
support(Supporter, Supported) :-
    graph_own(Ids), !,
    member(IdSupported, Ids),
    context_check(supportId(IdSupporter, IdSupported)),
    supportOf(IdSupporter, Supporter, IdSupported, Supported).
support(Supporter, Supported) :-
    context_check(supportId(IdSupporter, IdSupported)),
    supportOf(IdSupporter, Supporter, IdSupported, Supported).

supportOf(IdSupporter, Supporter, IdSupported, Supported) :-
    context_term(IdSupporter, argument(Supporter)),
    context_term(IdSupported, argument(Supported)).

%! supports(-Supports)
%  Supports are the (Supporter, Supported) pairs saved.
supports(Supports) :-
    findall((Supporter, Supported), support(Supporter, Supported), Supports).

%! supportersOf(+Argument, -Supporters)
%  Supporters are the saved arguments supporting Argument directly, by their identifiers.
supportersOf(Argument, Supporters) :-
    context_id(argument(Argument), Id),
    findall(S, (context_check(supportId(IdS, Id)), argumentById(IdS, S)), Supporters).

%--------------------------------------------------------------------------
% Attacks
%--------------------------------------------------------------------------

%! saveAttack(+Type, +Attacker, +Attacked, +On)
%  Attacker attacks Attacked on its sub-argument On (Attacked itself for a direct attack), with an attack of
%  Type, unless it already does.
saveAttack(Type, Attacker, Attacked, On) :-
    context_id(argument(Attacker), IdAttacker),
    context_id(argument(Attacked), IdAttacked),
    context_id(argument(On), IdOn),
    saveAttackById(Type, IdAttacker, Attacker, IdAttacked, Attacked, IdOn, On).

%! saveAttackById(+Type, +IdAttacker, +Attacker, +IdAttacked, +Attacked, +IdOn, +On)
%  As saveAttack/4, with the identifiers of the arguments known.
saveAttackById(Type, IdAttacker, Attacker, IdAttacked, Attacked, IdOn, On) :-
    context_absent(attackId(Type, IdAttacker, IdAttacked, IdOn)),
    context_assert(attackId(Type, IdAttacker, IdAttacked, IdOn)),
    graph_attack_saved(Type, IdAttacker, IdAttacked, IdOn).

%! removeAttack(+Type, +Attacker, +Attacked, +On)
%  The attack, saved (see saveAttack/4), is removed in all its forms (those extended from it with it); nothing
%  is done for an attack not saved (an extended one, say).
removeAttack(Type, Attacker, Attacked, On) :-
    dropAttack(Type, Attacker, Attacked, On, Fact),
    graph_removed([Fact]).

%! dropAttack(+Type, +Attacker, +Attacked, +On)
%  As removeAttack/4 in the active branch alone, not passed on to the other solvers of a session: for a branch made to
%  compute labels, dropped once they are read (see bp_grounded::argumentLabelling/0).
dropAttack(Type, Attacker, Attacked, On) :-
    dropAttack(Type, Attacker, Attacked, On, _).

dropAttack(Type, Attacker, Attacked, On, attackId(Type, IdAttacker, IdAttacked, IdOn)) :-
    context_id(argument(Attacker), IdAttacker),
    context_id(argument(Attacked), IdAttacked),
    context_id(argument(On), IdOn),
    context_retract(attackId(Type, IdAttacker, IdAttacked, IdOn)).

%! removeAttackIds(+Attacks)
%  Each of Attacks, a(Type, IdAttacker, IdAttacked), a direct attack, removed, all at once: one at a time, each would copy
%  what holds the others.
removeAttackIds(Attacks) :-
    findall(attackId(T, B, A, A), member(a(T, B, A), Attacks), Facts),
    context_retract_all(Facts),
    graph_removed(Facts).

%! attackFound(?Type, ?Attacker, ?Attacked, ?On)
%  The attack is saved, as it was found (attack/4 extends it): what context_check(attack(Type, Attacker, Attacked, On))
%  answers, as context_check(support(Supporter, Supported)) support/2.
attackFound(Type, Attacker, Attacked, On) :-
    context_check(attackId(Type, IdAttacker, IdAttacked, IdOn)),
    attackOf(IdAttacker, Attacker, IdAttacked, Attacked, IdOn, On).

attackOf(IdAttacker, Attacker, IdAttacked, Attacked, IdOn, On) :-
    context_term(IdAttacker, argument(Attacker)),
    context_term(IdAttacked, argument(Attacked)),
    context_term(IdOn, argument(On)).

%! attack(?Type, ?Attacker, ?Attacked, ?On)
%  Attacker attacks Attacked on On, with an attack of Type: saved, or extended to Attacked from On, which it is
%  built on.
attack(Type, Attacker, Attacked, On) :-
    expandedAttack(Type, Attacker, Attacked, On).

%! attacks(-Attacks)
%  Attacks are the (Type, Attacker, Attacked, On) attacks, saved or extended (see attack/4).
attacks(Attacks) :-
    findall((Type, Attacker, Attacked, On), attack(Type, Attacker, Attacked, On), Attacks).

%! attackById(?Type, ?IdAttacker, ?IdAttacked, ?IdOn)
%  The attack, saved or extended, by the identifiers of its arguments. Going through the attacks (IdAttacked
%  unbound): those on the arguments argumentById/2 goes through.
attackById(Type, IdAttacker, IdAttacked, IdOn) :-
    nonvar(IdAttacked), !,
    expandedAttackById(Type, IdAttacker, IdAttacked, IdOn).
attackById(Type, IdAttacker, IdAttacked, IdOn) :-
    graph_own(Ids), !,
    member(IdAttacked, Ids),
    expandedAttackById(Type, IdAttacker, IdAttacked, IdOn).
attackById(Type, IdAttacker, IdAttacked, IdOn) :-
    expandedAttackById(Type, IdAttacker, IdAttacked, IdOn).

%! attacksById(?IdAttacker, ?IdAttacked)
%  The argument with identifier IdAttacker attacks the one with identifier IdAttacked.
attacksById(IdAttacker, IdAttacked) :-
    expandedAttackById(_, IdAttacker, IdAttacked, _).

%! directAttack(?Type, ?Attacker, ?Attacked)
%  Attacker attacks Attacked itself, with an attack of Type: an attack saved, which attack/4 extends to the
%  arguments built on Attacked. The steps changing the attacks read these: the extended ones follow. (The
%  arguments are given as they are saved, as attack/4 gives them: removing an attack identifies them, see
%  context_id.)
directAttack(Type, Attacker, Attacked) :-
    context_check(attackId(Type, IdAttacker, IdAttacked, IdAttacked)),
    context_term(IdAttacker, argument(Attacker)),
    context_term(IdAttacked, argument(Attacked)).

%! directAttacks(-Attacks)
%  Attacks are the (Type, Attacker, Attacked) attacks saved (see directAttack/3): those on the arguments
%  argumentById/2 goes through (directAttack/3, a lookup, looks among all of them).
directAttacks(Attacks) :-
    graph_own(Ids), !,
    findall((Type, Attacker, Attacked), (
        member(IdAttacked, Ids),
        context_check(attackId(Type, IdAttacker, IdAttacked, IdAttacked)),
        context_term(IdAttacker, argument(Attacker)),
        context_term(IdAttacked, argument(Attacked))
    ), Attacks).
directAttacks(Attacks) :-
    findall((Type, Attacker, Attacked), directAttack(Type, Attacker, Attacked), Attacks).

%! reaches(+A, +B)
%  B is A or an argument reached from A through attacks: the arguments are visited breadth first, each once
%  (attack cycles are common, rebuttals being mutual).
reaches(A, B) :-
    argumentId(A, IdA),
    argumentId(B, IdB),
    reached([IdA], [], IdB), !.

reached([B|_], _, B) :- !.
reached([A|Frontier], Visited, B) :-
    findall(C, (attacksById(A, C), \+ member(C, [A|Visited]), \+ member(C, Frontier)), Next),
    append(Frontier, Next, Queue),
    reached(Queue, [A|Visited], B).

%--------------------------------------------------------------------------
% Labels
%--------------------------------------------------------------------------

%! labelArgument(+Label, +Argument)
%  Argument is labelled with Label (in, out or und), by itself and by its identifier.
labelArgument(Label, Argument) :-
    argumentId(Argument, Id),
    labelArgument(Label, Argument, Id).

%! labelArgument(+Label, +Argument, +Id)
%  Argument, with identifier Id, is labelled with Label, by itself and by its identifier.
labelArgument(Label, Argument, Id) :-
    labelFact(Label, Argument, Fact),
    context_assert(Fact),
    labelIdFact(Label, Id, FactId),
    context_assert(FactId).

%! unlabelArguments(+Label)
%  The arguments labelled with Label no longer are, by themselves and by their identifiers.
unlabelArguments(Label) :-
    labelFact(Label, _, Fact),
    context_retract(Fact),
    labelIdFact(Label, _, IdFact),
    context_retract(IdFact).

%! argumentLabelById(?Label, ?Id)
%  The argument with identifier Id is labelled with Label.
argumentLabelById(Label, Id) :-
    labelIdFact(Label, Id, Fact),
    context_check(Fact).

%! argumentLabelByIdIn(+Branch, ?Label, ?Id)
%  The argument with identifier Id is labelled with Label in the context branch Branch.
argumentLabelByIdIn(Branch, Label, Id) :-
    labelIdFact(Label, Id, Fact),
    context_check(Branch, Fact).

%! labelledIds(+Label, -Ids)
%  Ids are the identifiers of the arguments labelled with Label.
labelledIds(Label, Ids) :-
    findall(Id, argumentLabelById(Label, Id), Ids).

%! argumentLabel(?Label, ?Argument)
%  Argument is labelled with Label.
argumentLabel(Label, Argument) :-
    labelFact(Label, Argument, Fact),
    context_check(Fact).

%! labelStatement(+Label, +Statement)
%  Statement is labelled with Label, by the identifier of the label as well (see statementLabelled/2).
labelStatement(Label, Statement) :-
    statementLabelFact(Label, Statement, Fact),
    context_id(Fact, Key),
    context_assert_id(Key),
    context_assert(statementKey(Key)).

%! statementLabel(?Label, ?Statement)
%  Statement is labelled with Label.
statementLabel(Label, Statement) :-
    statementLabelFact(Label, Statement, Fact),
    context_check(Fact).

%! statementLabelled(+Label, +Statement)
%  Statement, as it is written (up to the names of its variables), is labelled with Label: unlike
%  statementLabel/2, a statement with variables is not labelled by those of its instances labelled, nor they by
%  it.
statementLabelled(Label, Statement) :-
    statementLabelFact(Label, Statement, Fact),
    context_id(Fact, Key),
    context_check(statementKey(Key)).

%! labelledArguments(+Label, -Arguments)
%  Arguments are those labelled with Label.
labelledArguments(Label, Arguments) :-
    findall(Argument, argumentLabel(Label, Argument), Arguments).

%--------------------------------------------------------------------------
% The graph and its labels in a given context branch, rather than the active one (read by Kotlin, see
% GraphMining)
%--------------------------------------------------------------------------

%! argumentIn(+Branch, ?Argument)
argumentIn(Branch, Argument) :-
    context_check(Branch, argument(Argument)).

%! supportIn(+Branch, ?Supporter, ?Supported)
supportIn(Branch, Supporter, Supported) :-
    context_check(Branch, supportId(IdSupporter, IdSupported)),
    supportOf(IdSupporter, Supporter, IdSupported, Supported).

%! attackFoundIn(+Branch, ?Type, ?Attacker, ?Attacked, ?On)
attackFoundIn(Branch, Type, Attacker, Attacked, On) :-
    context_check(Branch, attackId(Type, IdAttacker, IdAttacked, IdOn)),
    attackOf(IdAttacker, Attacker, IdAttacked, Attacked, IdOn, On).

%! attackIn(+Branch, ?Type, ?Attacker, ?Attacked, ?On)
attackIn(Branch, Type, Attacker, Attacked, On) :-
    expandedAttackIn(Branch, Type, Attacker, Attacked, On).

%! argumentLabelIn(+Branch, ?Label, ?Argument)
argumentLabelIn(Branch, Label, Argument) :-
    labelFact(Label, Argument, Fact),
    context_check(Branch, Fact).

%--------------------------------------------------------------------------
% The whole graph and its labels, sorted
%--------------------------------------------------------------------------

%! graph(-Arguments, -Attacks, -Supports)
%  The arguments, the (Type, A, B, C) attacks and the (A, B) supports, each sorted.
graph(Arguments, Attacks, Supports) :-
    arguments(AllArguments),
    attacks(AllAttacks),
    supports(AllSupports),
    utils::sortDescending(AllArguments, Arguments),
    utils::sortDescending(AllAttacks, Attacks),
    utils::sortDescending(AllSupports, Supports).

%! argumentLabelling(-In, -Out, -Und)
%  The arguments labelled IN, OUT and UND, each sorted.
argumentLabelling(In, Out, Und) :-
    sortedLabelled(argumentLabel, in, In),
    sortedLabelled(argumentLabel, out, Out),
    sortedLabelled(argumentLabel, und, Und).

%! statementLabelling(-In, -Out, -Und)
%  The statements labelled IN, OUT and UND, each sorted.
statementLabelling(In, Out, Und) :-
    sortedLabelled(statementLabel, in, In),
    sortedLabelled(statementLabel, out, Out),
    sortedLabelled(statementLabel, und, Und).

sortedLabelled(Kind, Label, Sorted) :-
    findall(X, labelledAs(Kind, Label, X), All),
    utils::sortDescending(All, Sorted).

labelledAs(argumentLabel, Label, Argument) :- argumentLabel(Label, Argument).
labelledAs(statementLabel, Label, Statement) :- statementLabel(Label, Statement).
