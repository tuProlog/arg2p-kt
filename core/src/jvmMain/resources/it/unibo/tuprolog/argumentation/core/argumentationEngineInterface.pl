% ----------------------------------------------------------------
% argumentationEngineInterface.pl
% PIKA-lab
% Year: 2019
% ---------------------------------------------------------------

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
    convertAllRules,
    computeGlobalAcceptance([STATIN, STATOUT, STATUND], [ARGSIN, ARGSOUT, ARGSUND]).

buildLabelSets(STATIN, STATOUT, STATUND) :-
    convertAllRules,
    computeGlobalAcceptance([STATIN, STATOUT, STATUND], [ARGSIN, ARGSOUT, ARGSUND]).

answerQuery(GOAL, YES, NO, UND) :-
    convertAllRules,
    computeStatementAcceptance(GOAL, YES, NO, UND).
