%! declared(?Statement, ?Other)
%  The theory declares Statement in conflict with Other (conflict/2, or conflict/3 whatever its guard), and the
%  conflict model counts the declared conflicts (see declaredCounted/0).
declared(Statement, Other) :-
    declaredCounted,
    (user::conflict(Statement, Other) ; user::conflict(Statement, Other, _)).

%! declaredBothWays(?Statement, ?Other)
%  The theory declares the statements in conflict both ways, with no guard, and the conflict model counts the declared
%  conflicts: contradictories, as a statement and its negation are.
declaredBothWays(Statement, Other) :-
    declaredCounted,
    user::conflict([Statement], [Other]),
    user::conflict([Other], [Statement]).

%! declaredGuarded(?Statement, ?Other, ?Guard)
%  The theory declares Statement in conflict with Other when Guard holds (conflict/3). Read as asked rather than stored:
%  a declaration may hold variables, and have a guard to check each time.
declaredGuarded(Statement, Other, Guard) :-
    user::conflict(Statement, Other, Guard).

%! declaredPlainly(?Statement, ?Other)
%  The theory declares Statement in conflict with Other, with no guard (conflict/2).
declaredPlainly(Statement, Other) :-
    user::conflict(Statement, Other).

%! languageConflict(?Statement, ?Other)
%  A language of the theory states Statement in conflict with Other (conflicts/1 of a library reading the theory, see
%  parser::extension/1), read the first time it is asked for.
languageConflict(Statement, Other) :-
    utils::oncePerTheory(languageConflicts, conflict::readLanguageConflicts),
    cacheCheck(conflict(Statement, Other)).

%! languageRuleConflict(?Statement, ?Other)
%  A language of the theory makes Statement conflict with Other by a rule of its own (conflictsBetween/2 of a library
%  reading the theory, see parser::extension/1): a conflict lifted to the statements the language builds, from the
%  conflicts of others (as temporal makes holdsAt(U, T) conflict with holdsAt(V, T) when U conflicts with V). The
%  languages defining one are read the first time it is asked for.
languageRuleConflict(Statement, Other) :-
    utils::oncePerTheory(languageConflictRules, conflict::readLanguageConflictRules),
    cacheCheck(conflictRules(Language)),
    Language::conflictsBetween(Statement, Other).

readLanguageConflictRules :-
    findall(_, (
        parser::extension(Language),
        moduleDefines(Language, conflictsBetween/2),
        cacheAssert(conflictRules(Language))
    ), _).

readLanguageConflicts :-
    findall(Conflicts, (
        parser::extension(Language),
        moduleDefines(Language, conflicts/1),
        Language::conflicts(Conflicts)
    ), Lists),
    utils::appendLists(Lists, All),
    utils::cacheAll(All).
