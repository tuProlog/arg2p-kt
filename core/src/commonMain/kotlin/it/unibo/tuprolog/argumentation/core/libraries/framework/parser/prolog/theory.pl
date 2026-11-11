%! readTheory(-Rules, -StrictIds)
%  The rules and premises of the theory, as the extensions of its language give them (see extension/1): Rules, the
%  defeasible rules, the strict rules, the ordinary premises and the axioms, in this order ([Id, Premises, Conclusion] for
%  a rule, Premises a conjunction, [Id, Statement] for a premise); StrictIds, strict(Id) for each strict rule and axiom.
%  The extensions defining rewrite/4 then turn them, once (see rewritten/4). What is read is kept by the store (see
%  store::rulesRead/0): the parser knows nothing of how.
readTheory(Rules, StrictIds) :-
    contributions(rules, DefeasibleRules),
    contributions(premises, Premises),
    contributions(axioms, Axioms),
    strictFacts(Axioms, AxiomsIds),
    utils::appendLists([DefeasibleRules, Premises, Axioms], Others),
    collectedStrictRules(Others, StrictRules, RulesIds),
    utils::appendLists([AxiomsIds, RulesIds], ReadIds),
    utils::appendLists([DefeasibleRules, StrictRules, Premises, Axioms], Read),
    rewritten(Read, ReadIds, Rules, StrictIds), !.

% rewritten(+Rules, +Ids, -Rewritten, -RewrittenIds): Rules and their strict/1 facts Ids as the extensions defining
% rewrite/4 turn them, each in turn, in their order (naturalTerms, the statements written as text into terms; metaRules,
% applicable(Id) a premise of each rule): what the store keeps, and what a theory partitioned is written with
rewritten(Rules, Ids, Rewritten, RewrittenIds) :-
    findall(Extension, (extension(Extension), moduleDefines(Extension, rewrite/4)), Extensions),
    rewrittenBy(Extensions, Rules, Ids, Rewritten, RewrittenIds).

rewrittenBy([], Rules, Ids, Rules, Ids).
rewrittenBy([Extension|Extensions], Rules, Ids, Rewritten, RewrittenIds) :-
    Extension::rewrite(Rules, Ids, Next, NextIds),
    rewrittenBy(Extensions, Next, NextIds, Rewritten, RewrittenIds).

%! extension(-Extension)
%  A library giving part of what is read of a theory, in the order their contributions take: the notation of the rules of
%  arg2p (ruleNotation), with no language stated; the graph extensions; the languages of the theory (graphLanguage, also the
%  ones of a user), mapping their notation to these kinds; and the libraries an extension when their flag is on
%  (extensionFlag/2: temporal, metaRules, conflictArguments with metaConflicts, ...), in the order of the registry.
%  Each defines the hooks it needs: rules/1 (defeasible, as [Id, Premises, Conclusion]), strictRules/3 (see
%  collectedStrictRules/3), premises/1 and axioms/1 (as [Id, Statement]), rewrite/4 (see rewritten/4), check/0 (see
%  checkLanguages/0). What only one library uses, it reads of the theory by itself, when it needs it. An extension
%  defining requires/1 brings the extensions it names after it (temporal needs the arguments for sets of values of
%  constraints).
extension(Extension) :-
    stated(Stated),
    (Extension = Stated ; required(Stated, Extension)).

stated(ruleNotation) :- \+ setting(graphLanguage, _).
stated(Extension) :- setting(graphExtension, Extension).
stated(Language) :- setting(graphLanguage, Language).
stated(Library) :- extensionFlag(Library, Flag), setting(Flag).

required(Extension, Required) :-
    moduleDefines(Extension, requires/1),
    Extension::requires(Direct),
    (Required = Direct ; required(Direct, Required)).

%! hook(+Predicate, -Module)
%  Module, the first extension (see extension/1) defining Predicate (Name/Arity), extends the base modules there: those
%  building the graph and answering queries call it where it does, and do as they do alone where none does. Found once
%  for the theory.
hook(Predicate, Module) :-
    cacheCheck(hookOf(Predicate, Found)), !,
    Found \== none,
    Module = Found.
hook(Predicate, Module) :-
    (extension(Found), moduleDefines(Found, Predicate) -> true ; Found = none),
    cacheAssert(hookOf(Predicate, Found)),
    Found \== none,
    Module = Found.

%! checkLanguages
%  The theory is written in each of the extensions of its language defining check/0: each raises an error if not.
checkLanguages :-
    findall(Extension, (extension(Extension), moduleDefines(Extension, check/0)), Extensions),
    utils::forEach(Extension, Extensions, Extension::check).

%! holds(+Check)
%  Check, a Prolog goal of the theory (a premise prolog(Check) of a rule, say), holds in the theory of the user.
holds(Check) :-
    (callable(Check) -> user::call(Check) ; user::Check).

% contributions(+Hook, -All): all that the extensions defining Hook/1 give, in their order
contributions(Hook, All) :-
    findall(Part, (
        extension(Extension),
        moduleDefines(Extension, Hook/1),
        Goal =.. [Hook, Part],
        Extension::Goal
    ), Parts),
    utils::appendLists(Parts, All).

% collectedStrictRules(+Others, -Rules, -Ids): the strict rules the extensions defining strictRules/3 give, each given those
% before it and the names taken, those of the rules so far and of Others, the other rules and premises of the theory (an
% extension may build on the strict rules, as transposition does); Ids are their strict/1 facts
collectedStrictRules(Others, Rules, Ids) :-
    ruleIds(Others, OtherIds),
    findall(Extension, (extension(Extension), moduleDefines(Extension, strictRules/3)), Extensions),
    strictRulesOf(Extensions, [], OtherIds, Rules),
    strictFacts(Rules, Ids).

strictRulesOf([], Rules, _, Rules).
strictRulesOf([Extension|Extensions], Rules, OtherIds, All) :-
    ruleIds(Rules, Ids),
    append(Ids, OtherIds, Taken),
    Extension::strictRules(Rules, Taken, Added),
    append(Rules, Added, Next),
    strictRulesOf(Extensions, Next, OtherIds, All).

% ruleIds(+Rules, -Ids): the identifiers of Rules, the lists [Id, ...], in their order
ruleIds([], []).
ruleIds([[Id|_]|Rules], [Id|Ids]) :- !, ruleIds(Rules, Ids).
ruleIds([_|Rules], Ids) :- ruleIds(Rules, Ids).

% strictFacts(+Rules, -Facts): strict(Id) for the identifier of each of Rules
strictFacts([], []).
strictFacts([[Id|_]|Rules], [strict(Id)|Facts]) :- strictFacts(Rules, Facts).
