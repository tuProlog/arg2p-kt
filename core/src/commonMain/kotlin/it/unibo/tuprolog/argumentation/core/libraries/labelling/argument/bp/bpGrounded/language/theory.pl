%! burden(?Literals)
%  The theory states a burden of persuasion on Literals, a list (bp/N facts), read the first time it is asked for.
burden(Literals) :-
    utils::oncePerTheory(burdens, bp_grounded::readBurdens),
    cache_check(abstractBp(Literals)).

readBurdens :-
    theoryBurdens(Burdens),
    utils::cacheAll(Burdens).
