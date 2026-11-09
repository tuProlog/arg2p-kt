%! check
%  The preference mode of ABA+ alone: a theory in the language of ABA.
check :-
    \+ setting(graphLanguage, abaLanguage), !,
    throw(error(domain_error(aba_theory, abaPref), 'graphExtension: abaPref is the preference mode of ABA theories alone (graphLanguage(abaLanguage), as the profiles aba and aba_plus state)')).
check.
