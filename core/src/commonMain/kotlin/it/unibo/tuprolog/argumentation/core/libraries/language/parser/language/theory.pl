%! rulesRead
%  The rules and premises of the theory read, once for the theory (see utils::oncePerTheory/2).
rulesRead :- utils::oncePerTheory(rules, parser::readRules).

%! readRules
%  What the extensions of the language give of the rules and premises (see extension/1), as the clauses of the module
%  rule and strict/1 facts, in the cache. The term as large as the theory is not kept bound: it weighs on every binding
%  made after it.
readRules :- \+ \+ readRules(_).

readRules(Stored) :-
    contributions(rules, DefeasibleRules),
    contributions(premises, Premises),
    contributions(axioms, Axioms),
    strictFacts(Axioms, AxiomsIds),
    utils::appendLists([DefeasibleRules, Premises, Axioms], Others),
    collectedStrictRules(Others, StrictRules, RulesIds),
    utils::appendLists([AxiomsIds, RulesIds], StrictIds),
    utils::appendLists([DefeasibleRules, StrictRules, Premises, Axioms], Rules),
    clausesOf(Rules, Clauses, StrictIds, MappedIds),
    utils::appendLists([Clauses, MappedIds], Stored),
    utils::cacheAll(Stored), !.

%! extension(-Extension)
%  A library giving part of what is read of a theory, in the order their contributions take: the notation of arg2p as it
%  is (below), with no language stated; the graph extensions; the languages of the theory (graphLanguage, also the ones of
%  a user), mapping their notation to these kinds; and the libraries declaring they read a theory (parsing_library/2),
%  in the order of the registry.
%  Each defines the hooks it needs: rules/1 (defeasible, as [Id, Premises, Conclusion]), strictRules/3 (see
%  collectedStrictRules/3), premises/1 and axioms/1 (as [Id, Statement]), clauses/4 (see clausesOf/4), check/0 (see
%  checkLanguages/0). What only one library uses, it reads of the theory by itself, when it needs it.
extension(parser) :- \+ setting(graphLanguage, _).
extension(Extension) :- setting(graphExtension, Extension).
extension(Language) :- setting(graphLanguage, Language).
extension(Library) :- parsing_library(Library, Flag), setting(Flag).

%! contributions(+Hook, -All)
%  All that the extensions defining Hook/1 give, in their order.
contributions(Hook, All) :-
    findall(Part, (
        extension(Extension),
        module_defines(Extension, Hook/1),
        Goal =.. [Hook, Part],
        Extension::Goal
    ), Parts),
    utils::appendLists(Parts, All).

%! clausesOf(+Rules, -Clauses, +StrictIds, -MappedIds)
%  The clauses of Rules, as the module rule writes them: as an extension defining clauses/4 turns them, with the strict/1
%  facts of StrictIds for the rules as it names them; plainly otherwise.
clausesOf(Rules, Clauses, StrictIds, MappedIds) :-
    extension(Extension),
    module_defines(Extension, clauses/4), !,
    Extension::clauses(Rules, Clauses, StrictIds, MappedIds).
clausesOf(Rules, Clauses, StrictIds, StrictIds) :-
    ruleToClause(Rules, Clauses).

%! collectedStrictRules(+Others, -Rules, -Ids)
%  The strict rules the extensions defining strictRules/3 give, each given those before it and the names taken, those of
%  the rules so far and of Others, the other rules and premises of the theory (an extension may build on the strict rules,
%  as transposition does); Ids are their strict/1 facts.
collectedStrictRules(Others, Rules, Ids) :-
    ruleIds(Others, OtherIds),
    findall(Extension, (extension(Extension), module_defines(Extension, strictRules/3)), Extensions),
    strictRulesOf(Extensions, [], OtherIds, Rules),
    strictFacts(Rules, Ids).

strictRulesOf([], Rules, _, Rules).
strictRulesOf([Extension|Extensions], Rules, OtherIds, All) :-
    ruleIds(Rules, Ids),
    append(Ids, OtherIds, Taken),
    Extension::strictRules(Rules, Taken, Added),
    append(Rules, Added, Next),
    strictRulesOf(Extensions, Next, OtherIds, All).

%! checkLanguages
%  The theory is written in each of the extensions of its language defining check/0: each raises an error if not.
checkLanguages :-
    findall(Extension, (extension(Extension), module_defines(Extension, check/0)), Extensions),
    utils::forEach(Extension, Extensions, Extension::check).

% ruleIds(+Rules, -Ids): the identifiers of Rules, the lists [Id, ...], in their order
ruleIds([], []).
ruleIds([[Id|_]|Rules], [Id|Ids]) :- !, ruleIds(Rules, Ids).
ruleIds([_|Rules], Ids) :- ruleIds(Rules, Ids).

% strictFacts(+Rules, -Facts): strict(Id) for the identifier of each of Rules
strictFacts([], []).
strictFacts([[Id|_]|Rules], [strict(Id)|Facts]) :- strictFacts(Rules, Facts).

%==============================================================================
% THE NOTATION OF ARG2P: what a theory writes of each kind, for the languages using it (or for the parser, with no
% language stated), which connect to the hooks of the parser the parts of it they have
%==============================================================================

%! rules(-Rules)
%  The defeasible rules of the theory.
rules(Rules) :-
    findall([RuleName, Preconditions, Effect], user::(RuleName : Preconditions => Effect), Rules).

%! strictRules(+Rules, +Taken, -Added)
%  The strict rules of the theory.
strictRules(_, _, Added) :-
    findall([RuleName, Preconditions, Effect], user::(RuleName : Preconditions -> Effect), Added).

%! premises(-Premises)
%  The ordinary premises of the theory, as [Id, Statement].
premises(Premises) :-
    findall([RuleName, Effect], user::(RuleName :=> Effect), Premises).

%! axioms(-Axioms)
%  The axioms of the theory, as [Id, Statement].
axioms(Axioms) :-
    findall([RuleName, Effect], user::(RuleName :-> Effect), Axioms).
