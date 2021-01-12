load_module(Module, Theory) :-
	modulesPath(X),
	strings_concat([X, '/', Module, '.pl'], Path),
	text_from_file(Path, Theory).

load_modules([], []).
load_modules([H|T], [Theory|X]) :-
	load_module(H, Theory),
	load_modules(T, X).

call_module(Modules, Query) :-
	load_modules(Modules, Theories),
	strings_concat(Theories, '\n\n', ModuleTheory),
	arg_agent(ModuleTheory, Query, X), !,
	member(Query, X).

strings_concat(X, Y) :- strings_concat(X, '', Y).

strings_concat([], _, '').
strings_concat([H|T], D, Y) :-
	strings_concat(T, X),
	atom_concat(H, D, Z),
	atom_concat(Z, X, Y).