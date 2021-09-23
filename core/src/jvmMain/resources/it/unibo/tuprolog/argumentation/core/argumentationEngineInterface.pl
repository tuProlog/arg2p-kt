argTuProlog.

buildLabelSets([STATIN, STATOUT, STATUND], [ARGSIN, ARGSOUT, ARGSUND]) :-
    parser::convertAllRules(ArgRules),
    abstract::computeGlobalAcceptance(ArgRules, X, [STATIN, STATOUT, STATUND], [ARGSIN, ARGSOUT, ARGSUND]),
    write(X).

buildLabelSets(STATIN, STATOUT, STATUND) :-
    parser::convertAllRules(ArgRules),
    abstract::computeGlobalAcceptance(ArgRules, _, [STATIN, STATOUT, STATUND], _).

buildLabelSets :-
    buildLabelSets([STATIN, STATOUT, STATUND], [ARGSIN, ARGSOUT, ARGSUND]),
    debug::printArgumentLabelling([ARGSIN, ARGSOUT, ARGSUND]),
    debug::printStatementLabelling([STATIN, STATOUT, STATUND]).

answerQuery(GOAL, YES, NO, UND) :-
    parser::convertAllRules(ArgRules),
    debug::printTheory(ArgRules),
    structured::computeStatementAcceptance(ArgRules, GOAL, YES, NO, UND).
