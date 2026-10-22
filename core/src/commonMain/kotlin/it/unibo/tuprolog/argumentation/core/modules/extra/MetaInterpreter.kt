package it.unibo.tuprolog.argumentation.core.modules.extra

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

object MetaInterpreter : PrologModule("prolog.argumentation.meta.crossjustice", "interpreter") {
    override val prologRawTheory: String =
        """
        % with_facts_and_length/3
        with_facts_and_length(X, Y, L) :-
            solve(X, R),
            recover_facts(R, Y),
            length(Y, L).
        
        % recover_facts/2
        
        recover_facts([], []) :- !.
        recover_facts([H|T], C) :-
            is_list(H), !,
            recover_facts(T, TC),
            recover_facts(H, HC),
            append(TC, HC, C).
        recover_facts([H|T], C) :-
            \+ is_list(H),
            recover_facts(T, TC),
            evaluate(H, HC),
            append(TC, HC, C).
        
        evaluate(user_fact(X), [X]) :- !.
        evaluate(_, []) :- !.
        
        % solve/2
        
        solve((A,B), Result) :- !,
            solve(A, ARes),
            solve(B, BRes),
            append(ARes, BRes, Result).
        
        solve((A;B), Result) :- !,
            solve(A, Result);
            solve(B, Result).
        
        solve(member(A, B), [system_predicate]) :- !,
            call(member(A, B)).
        
        solve(\+(A), [not(A)]) :- !,
            call(\+(A)).
        
        solve((A)\=(B), [doNotUnify(A, B)]) :- !,
            call((A)\=(B)).
        
        solve(A, [system_predicate]) :-
            catch(clause(A, _), B, true),
            \+ var(B), !,
            call(A).
        
        solve(A, [A]) :-
            \+ clause(A, B), !,
            call(A).
        
        solve(A, [A|[Res]]) :-
            clause(A, B),
            solve(B, Res).
        
        % is_list/1
        
        is_list(X) :- var(X), !, fail.
        is_list([]).
        is_list([_|T]) :- is_list(T).
        
        % fetch_argument_facts/2
        
        fetch_argument_facts(Conclusion, Facts) :-
            argument([R, TR, [Conclusion]]),
            findall(user_fact(Fact), 
                support([_, _, [user_fact(Fact)]], [R, TR, [Conclusion]]),
            Facts).
        """.trimIndent()
}
