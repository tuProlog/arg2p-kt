package it.unibo.tuprolog.argumentation.core.modules.utils

import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.utils.AppendFast
import it.unibo.tuprolog.argumentation.core.primitives.utils.AppendLists
import it.unibo.tuprolog.argumentation.core.primitives.utils.AssertAll
import it.unibo.tuprolog.argumentation.core.primitives.utils.Contains
import it.unibo.tuprolog.argumentation.core.primitives.utils.ContainsAny
import it.unibo.tuprolog.argumentation.core.primitives.utils.Deduplicate
import it.unibo.tuprolog.argumentation.core.primitives.utils.ForEach
import it.unibo.tuprolog.argumentation.core.primitives.utils.Intersection
import it.unibo.tuprolog.argumentation.core.primitives.utils.MinimalBySet
import it.unibo.tuprolog.argumentation.core.primitives.utils.Sccs
import it.unibo.tuprolog.argumentation.core.primitives.utils.Sort
import it.unibo.tuprolog.argumentation.core.primitives.utils.SortDistinct
import it.unibo.tuprolog.argumentation.core.primitives.utils.Subtract
import it.unibo.tuprolog.argumentation.core.primitives.utils.UniqueByFirst
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * Utilities on lists and on the labels of the context (module `utils`), most of them native primitives.
 */
object UtilsModule : PrologModule("prolog.argumentation.utils", "utils") {
    override val primitives: Map<Signature, Primitive>
        get() =
            mapOf(
                AppendFast.signature to AppendFast,
                AppendLists.signature to AppendLists,
                AssertAll.signature to AssertAll,
                ForEach.signature to ForEach,
                Contains.signature to Contains,
                ContainsAny.signature to ContainsAny,
                Sort.descriptionPair,
                Deduplicate.descriptionPair,
                SortDistinct.descriptionPair,
                MinimalBySet.descriptionPair,
                UniqueByFirst.descriptionPair,
                Intersection.descriptionPair,
                Subtract.descriptionPair,
                Sccs(1).let { it.signature to it },
                Sccs(2).let { it.signature to it },
            )

    override val prologRawTheory: String =
        // language=Prolog
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



        % sortDistinct/2, deduplicate/2, sort/2 and subtract/3 are native primitives (see primitives/utils)


        isEmptyList([]).


        % Misc utils

        % search(+F, +L, -S): S is each term F(...) of arity 1 to L that holds
        search(F, L, S) :-
           between(1, L, N),
           functor(S, F, N),
           call(S).
        """.trimIndent()
}
