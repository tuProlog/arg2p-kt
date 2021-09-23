argTuProlog.

buildLabelSets :-
    buildLabelSets([STATIN, STATOUT, STATUND], [ARGSIN, ARGSOUT, ARGSUND]),
    write('\n=================================================>'),nl,
    write('\nSTATEMENT LABELLING  ============================>'),nl,
    write('\n=================================================>'),nl,
    write('IN ==============================================>'),nl,
    writeList(STATIN),
    write('OUT =============================================>'),nl,
    writeList(STATOUT),
    write('UND =============================================>'),nl,
    writeList(STATUND),
    write('\n=================================================>'),nl,
    write('\nARGUMENT LABELLING ==============================>'),nl,
    write('\n=================================================>'),nl,
    write('IN ==============================================>'),nl,
    writeList(ARGSIN),
    write('OUT =============================================>'),nl,
    writeList(ARGSOUT),
    write('UND =============================================>'),nl,
    writeList(ARGSUND),
    write('=================================================>'),nl.

buildLabelSets([STATIN, STATOUT, STATUND], [ARGSIN, ARGSOUT, ARGSUND]) :-
    parser::convertAllRules(ArgRules),
    abstract::computeGlobalAcceptance(ArgRules, [STATIN, STATOUT, STATUND], [ARGSIN, ARGSOUT, ARGSUND]).

buildLabelSets(STATIN, STATOUT, STATUND) :-
    parser::convertAllRules(ArgRules),
    abstract::computeGlobalAcceptance(ArgRules, [STATIN, STATOUT, STATUND], _).

answerQuery(GOAL, YES, NO, UND) :-
    parser::convertAllRules(ArgRules),
    structured::computeStatementAcceptance(ArgRules, GOAL, YES, NO, UND).
