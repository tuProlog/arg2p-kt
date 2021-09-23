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
    parser::convertAllRules,
    abstract::computeGlobalAcceptance([STATIN, STATOUT, STATUND], [ARGSIN, ARGSOUT, ARGSUND]).

buildLabelSets(STATIN, STATOUT, STATUND) :-
    parser::convertAllRules,
    abstract::computeGlobalAcceptance([STATIN, STATOUT, STATUND], _).

answerQuery(GOAL, YES, NO, UND) :-
    parser::convertAllRules,
    structured::computeStatementAcceptance(GOAL, YES, NO, UND).
