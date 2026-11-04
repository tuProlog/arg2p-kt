package it.unibo.tuprolog.argumentation.core.modules.utils

import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.utils.Deduplicate
import it.unibo.tuprolog.argumentation.core.primitives.utils.Intersection
import it.unibo.tuprolog.argumentation.core.primitives.utils.MinimalBySet
import it.unibo.tuprolog.argumentation.core.primitives.utils.Sccs
import it.unibo.tuprolog.argumentation.core.primitives.utils.Sort
import it.unibo.tuprolog.argumentation.core.primitives.utils.SortDistinct
import it.unibo.tuprolog.argumentation.core.primitives.utils.UniqueByFirst
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * Utilities on lists and on the labels of the context (module `utils`).
 */
object UtilsModule : PrologModule("prolog.argumentation.utils", "utils") {
    override val primitives: Map<Signature, Primitive>
        get() =
            mapOf(
                Sort.signature to Sort,
                Deduplicate.signature to Deduplicate,
                SortDistinct.signature to SortDistinct,
                MinimalBySet.signature to MinimalBySet,
                UniqueByFirst.signature to UniqueByFirst,
                Intersection.signature to Intersection,
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

        %! forEach(?Element, +List, +Goal)
        %  Goal, all its solutions, for each element of List. Goal runs in this module: other modules are called through ::.
        forEach(Element, List, Goal) :-
            findall(_, (member(Element, List), Goal), _).

        %! assertAll(+Clauses)
        %  Each of Clauses asserted in the context, in order.
        assertAll(Clauses) :-
            findall(_, (member(Clause, Clauses), context_assert(Clause)), _).

        %! appendLists(+Lists, -Result)
        %  Result is the concatenation of the lists in Lists.
        appendLists([], []).
        appendLists([List|Lists], Result) :-
            append(List, Rest, Result),
            appendLists(Lists, Rest).

        %! contains(?Element, +List)
        %  An element of List matches Element (which is not bound).
        contains(Element, List) :-
            \+ \+ member(Element, List).

        %! containsAny(+Elements, +List)
        %  An element of List matches one of Elements (which are not bound).
        containsAny(Elements, List) :-
            \+ \+ (member(Element, List), member(Element, Elements)).

        %! subtract(+List, +Removed, -Rest)
        %  Rest is the elements of List not in Removed, in order.
        subtract([], _, []).
        subtract([Element|Elements], Removed, Rest) :-
            identicalTo(Element, Removed), !,
            subtract(Elements, Removed, Rest).
        subtract([Element|Elements], Removed, [Element|Rest]) :-
            subtract(Elements, Removed, Rest).

        identicalTo(Element, [Other|_]) :- Element == Other, !.
        identicalTo(Element, [_|Others]) :- identicalTo(Element, Others).

        % The others (sortDistinct/2, deduplicate/2, sortDescending/2, intersectionOf/2, ...) are native primitives
        """.trimIndent()
}
