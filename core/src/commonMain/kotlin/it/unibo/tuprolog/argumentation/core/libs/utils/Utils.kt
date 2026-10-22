package it.unibo.tuprolog.argumentation.core.libs.utils

import it.unibo.tuprolog.argumentation.core.libs.PrologModule
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * Utilities on lists and on the labels of the context (module `utils`), most of them native primitives (see this file).
 */
object Utils : PrologModule("prolog.argumentation.utils", "utils") {
    override val primitives: Map<Signature, Primitive>
        get() =
            mapOf(
                AppendOptimized.signature to AppendOptimized,
                AppendLists.signature to AppendLists,
                AssertAll.signature to AssertAll,
                ForEach.signature to ForEach,
                Contains.signature to Contains,
                ContainsAny.signature to ContainsAny,
                SortTerms.descriptionPair,
                Deduplicate.descriptionPair,
                SortDistinct.descriptionPair,
                MinimalBySet.descriptionPair,
                UniqueByFirst.descriptionPair,
                Intersection.descriptionPair,
                Subtract.descriptionPair,
                StronglyConnectedComponents(1).let { it.signature to it },
                StronglyConnectedComponents(2).let { it.signature to it },
            )

    override val prologRawTheory: String =
        """
        % Lists Utils

        %! writeList(+List)
        %  The items of List written one after the other, then a new line.
        writeList([]) :- nl.
        writeList([X|Others]) :-
            write(X),
            writeList(Others).


        %! writeListNl(+List)
        %  The items of List written each on a line.
        writeListNl([]).
        writeListNl([X|Others]) :-
            write(X),nl,
            writeListNl(Others).



        % sortDistinct/2, deduplicate/2 and sort/2 are native primitives (see Utils.kt)

        % subtract/3 is a native primitive (see Utils.kt)


        isEmptyList([]).


        % Misc utils

        % search(+F, +L, -S): S is each term F(...) of arity 1 to L that holds
        search(F, L, S) :-
           between(1, L, N),
           functor(S, F, N),
           call(S).



        %! recoverGraph(-Arguments, -Attacks, -Supports)
        %  The graph in the context: arguments, (Type, A, B, C) attacks and (A, B) supports, each sorted.
        recoverGraph(Args, Attacks, Supports) :-
                findall(X, context_check(argument(X)), TempArgs),
                findall((T, A, B, C), context_check(attack(T, A, B, C)), TempAttacks),
                findall((A, B), context_check(support(A, B)), TempSupports),
                utils::sort(TempArgs, Args),
                utils::sort(TempAttacks, Attacks),
                utils::sort(TempSupports, Supports).

        %! recoverArgumentLabelling(-In, -Out, -Und)
        %  The arguments labelled IN, OUT and UND in the context, each sorted.
        recoverArgumentLabelling(ArgsIn, ArgsOut, ArgsUnd) :-
                findall(X, context_check(in(X)), TempArgsIn),
                findall(X, context_check(out(X)), TempArgsOut),
                findall(X, context_check(und(X)), TempArgsUnd),
                utils::sort(TempArgsIn, ArgsIn),
                utils::sort(TempArgsOut, ArgsOut),
                utils::sort(TempArgsUnd, ArgsUnd).

        %! recoverStatementLabelling(-In, -Out, -Und)
        %  The statements labelled IN, OUT and UND in the context, each sorted.
        recoverStatementLabelling(In, Out, Und) :-
                findall(X, context_check(statIn(X)), TempIn),
                findall(X, context_check(statOut(X)), TempOut),
                findall(X, context_check(statUnd(X)), TempUnd),
                utils::sort(TempIn, In),
                utils::sort(TempOut, Out),
                utils::sort(TempUnd, Und).

        %! recoverArgumentLabellingAbstract(-In, -Out, -Und)
        %  The claims of the arguments labelled IN, OUT and UND in the context (the abstract arguments, see abstract::solve/5), each sorted.
        recoverArgumentLabellingAbstract(ArgsIn, ArgsOut, ArgsUnd) :-
                findall(X, (argument::claim(A, X), context_check(in(A))), TempArgsIn),
                findall(X, (argument::claim(A, X), context_check(out(A))), TempArgsOut),
                findall(X, (argument::claim(A, X), context_check(und(A))), TempArgsUnd),
                utils::sort(TempArgsIn, ArgsIn),
                utils::sort(TempArgsOut, ArgsOut),
                utils::sort(TempArgsUnd, ArgsUnd).
        """.trimIndent()
}
