%! buildArgumentationGraph
%  The arguments of the theory and the attacks between them.
buildArgumentationGraph :-
    buildArguments,
    buildAttacks.

%! buildArguments
%  The arguments of the theory.
buildArguments :-
    argumentBuilder::buildArguments.

%! buildAttacks
%  The attacks between the saved arguments: all of them, or, in query mode, those explored (see the module
%  structuredPartial).
buildAttacks :-
    attackBuilder::buildAttacks.
