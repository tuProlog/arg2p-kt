package it.unibo.tuprolog.argumentation.core.modules.extra

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * A meta-interpreter tracing the facts a proof relies on (module `interpreter`), written for the CrossJustice project:
 * the facts of the user, `user_fact(Fact)`, a goal is proved with.
 */
object InterpreterModule : PrologModule("prolog.argumentation.meta.crossjustice", "interpreter") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! with_facts_and_length(+Goal, -Facts, -Length)
        %  Goal is proved, Facts being the facts of the user its proof relies on (user_fact(Fact)), and Length their
        %  number.
        with_facts_and_length(X, Y, L) :-
            solve(X, R),
            recover_facts(R, Y),
            length(Y, L).
        
        %! recover_facts(+Proof, -Facts)
        %  Facts are the facts of the user in Proof, as solve/2 gives it.
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
        
        %! solve(+Goal, -Proof)
        %  Goal is proved, Proof being the goals its proof goes through, nested as the clauses used.
        solve((A,B), Result) :- !,
            solve(A, ARes),
            solve(B, BRes),
            append(ARes, BRes, Result).
        
        solve((A;B), Result) :- !,
            (solve(A, Result) ; solve(B, Result)).
        
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
        
        %! is_list(?Term)
        %  Term is a list.
        is_list(X) :- var(X), !, fail.
        is_list([]).
        is_list([_|T]) :- is_list(T).
        
        %! fetch_argument_facts(+Conclusion, -Facts)
        %  Facts are the facts of the user supporting the argument for Conclusion, from argument/1 and support/2
        %  facts on arguments written as [Rules, TopRule, Conclusion].
        fetch_argument_facts(Conclusion, Facts) :-
            argument([R, TR, [Conclusion]]),
            findall(user_fact(Fact), 
                support([_, _, [user_fact(Fact)]], [R, TR, [Conclusion]]),
            Facts).
        """.trimIndent()
}
