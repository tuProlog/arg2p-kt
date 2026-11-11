%! preference(?Rule, ?Other)
%  Rule is preferred to Other: those stated in their place (see statePreferences/1), or the preferences the theory states
%  (sup/2), closed transitively (see supClosure/2), read the first time they are asked for.
preference(Rule, Other) :-
    contextCheck(preferencesStated), !,
    contextCheck(sup(Rule, Other)).
preference(Rule, Other) :-
    utils::oncePerTheory(preferences, superiority::readPreferences),
    cacheCheck(sup(Rule, Other)).

readPreferences :-
    findall(sup(X, Y), user::sup(X, Y), Stated),
    supClosure(Stated, Preferences),
    utils::cacheAll(Preferences).

%! statePreferences(+Preferences)
%  Preferences, sup(Rule, Other) terms, as the preferences, in place of those stated so far: the extension
%  defeasiblePref states the preferences concluded by arguments, which alone count then.
statePreferences(Preferences) :-
    contextRetract(sup(_, _)),
    findall(_, (member(Preference, Preferences), contextAssert(Preference)), _),
    (contextAbsent(preferencesStated) -> contextAssert(preferencesStated) ; true).

% The closure of ground preferences: a round joins the preferences last added (Fresh) with all of them, in the
% store, by either end: a pair of older ones gave what it gives in an earlier round
groundSupClosure(Sup, Fresh, Closed) :-
    findall(sup(X, Z), (
        (
            member(sup(X, Y), Fresh),
            contextCheck(supEdge(Y, Z))
        ;
            member(sup(Y, Z), Fresh),
            contextCheck(supEdge(X, Y))
        ),
        X \== Z,
        contextAbsent(supEdge(X, Z))
    ), New),
    utils::deduplicate(New, Added),
    (Added == [] ->
        Closed = Sup
    ;
        findall(_, (member(sup(X, Z), Added), contextAssert(supEdge(X, Z))), _),
        append(Sup, Added, Next),
        groundSupClosure(Next, Added, Closed)
    ).

%! supClosure(+Stated, -Closed)
%  The preferences stated, closed transitively: the ordering of rules and premises is a strict partial order
%  (no sup(X, X) is added, should the stated ones make a cycle). The preferences concluded by arguments
%  (defeasiblePref) are closed by a rule of the theory instead, as sup(X, Y), sup(Y, Z) -> sup(X, Z).
supClosure(Sup, Closed) :-
    ground(Sup), !,
    findall(_, (member(sup(X, Y), Sup), contextAssert(supEdge(X, Y))), _),
    groundSupClosure(Sup, Sup, Closed),
    contextRetract(supEdge(_, _)).
supClosure(Sup, Closed) :-
    findall(sup(X, Z), (
        member(sup(X, Y), Sup),
        member(sup(Y, Z), Sup),
        X \== Z,
        \+ member(sup(X, Z), Sup)
    ), New),
    utils::deduplicate(New, Fresh),
    (Fresh == [] -> Closed = Sup ; append(Sup, Fresh, Next), supClosure(Next, Closed)).
