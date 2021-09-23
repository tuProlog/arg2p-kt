argTuProlog.

buildLabelSets([STATIN, STATOUT, STATUND], [ARGSIN, ARGSOUT, ARGSUND]) :-
    parser::convertAllRules(ArgRules),
    abstract::computeGlobalAcceptance(ArgRules, [STATIN, STATOUT, STATUND], [ARGSIN, ARGSOUT, ARGSUND]).

buildLabelSets(STATIN, STATOUT, STATUND) :-
    parser::convertAllRules(ArgRules),
    abstract::computeGlobalAcceptance(ArgRules, [STATIN, STATOUT, STATUND], _).

buildLabelSets :-
    buildLabelSets([STATIN, STATOUT, STATUND], [ARGSIN, ARGSOUT, ARGSUND]),
    debug::printArgumentLabelling([ARGSIN, ARGSOUT, ARGSUND]),
    debug::printStatementLabelling([STATIN, STATOUT, STATUND]).

answerQuery(GOAL, YES, NO, UND) :-
    parser::convertAllRules(ArgRules),
    debug::printTheory(ArgRules),
    structured::computeStatementAcceptance(ArgRules, GOAL, YES, NO, UND).
