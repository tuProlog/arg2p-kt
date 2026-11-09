%! preference(?Rule, ?Other)
%  Rule is preferred to Other: those stated in their place (see statePreferences/1), or the preferences the theory states
%  (sup/2), closed transitively (see supClosure/2), read the first time they are asked for.
preference(Rule, Other) :-
    context_check(preferencesStated), !,
    context_check(sup(Rule, Other)).
preference(Rule, Other) :-
    utils::oncePerTheory(preferences, superiority::readPreferences),
    cache_check(sup(Rule, Other)).

readPreferences :-
    findall(sup(X, Y), user::sup(X, Y), Stated),
    supClosure(Stated, Preferences),
    utils::cacheAll(Preferences).

%! statePreferences(+Preferences)
%  Preferences, sup(Rule, Other) terms, as the preferences, in place of those stated so far: the extension
%  defeasiblePref states the preferences concluded by arguments, which alone count then.
statePreferences(Preferences) :-
    context_retract(sup(_, _)),
    findall(_, (member(Preference, Preferences), context_assert(Preference)), _),
    (context_absent(preferencesStated) -> context_assert(preferencesStated) ; true).

% The closure of ground preferences: a round joins the preferences last added (Fresh) with all of them, in the
% store, by either end: a pair of older ones gave what it gives in an earlier round
groundSupClosure(Sup, Fresh, Closed) :-
    findall(sup(X, Z), (
        (
            member(sup(X, Y), Fresh),
            context_check(supEdge(Y, Z))
        ;
            member(sup(Y, Z), Fresh),
            context_check(supEdge(X, Y))
        ),
        X \== Z,
        context_absent(supEdge(X, Z))
    ), New),
    utils::deduplicate(New, Added),
    (Added == [] ->
        Closed = Sup
    ;
        findall(_, (member(sup(X, Z), Added), context_assert(supEdge(X, Z))), _),
        append(Sup, Added, Next),
        groundSupClosure(Next, Added, Closed)
    ).

%! supClosure(+Stated, -Closed)
%  The preferences stated, closed transitively: the ordering of rules and premises is a strict partial order
%  (no sup(X, X) is added, should the stated ones make a cycle). The preferences concluded by arguments
%  (defeasiblePref) are closed by a rule of the theory instead, as sup(X, Y), sup(Y, Z) -> sup(X, Z).
supClosure(Sup, Closed) :-
    ground(Sup), !,
    findall(_, (member(sup(X, Y), Sup), context_assert(supEdge(X, Y))), _),
    groundSupClosure(Sup, Sup, Closed),
    context_retract(supEdge(_, _)).
supClosure(Sup, Closed) :-
    findall(sup(X, Z), (
        member(sup(X, Y), Sup),
        member(sup(Y, Z), Sup),
        X \== Z,
        \+ member(sup(X, Z), Sup)
    ), New),
    utils::deduplicate(New, Fresh),
    (Fresh == [] -> Closed = Sup ; append(Sup, Fresh, Next), supClosure(Next, Closed)).
