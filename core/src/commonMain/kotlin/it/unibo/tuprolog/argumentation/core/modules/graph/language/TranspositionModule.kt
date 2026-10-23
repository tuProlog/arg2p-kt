package it.unibo.tuprolog.argumentation.core.modules.graph.language

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The transpositions of the strict rules (module `transposition`), with autoTransposition: for a strict rule
 * `r : a, b -> c`, the rules concluding the negation of each premise from the negation of the conclusion and the
 * other premises (`r_i : -c, b -> -a`, `r_ii : -c, a -> -b`).
 */
object TranspositionModule : PrologModule("prolog.argumentation.language.transposition", "transposition") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! transpose(+Rules, +StrictRules, -Transposed)
        %  Transposed are StrictRules with the transpositions of Rules, with autoTransposition; StrictRules otherwise.
        transpose(_, CtrRules, CtrRules) :- \+ setting(autoTransposition), !.
        % The rules followed by the transpositions of each one, from the last rule to the first, keeping the last rule
        % with each name: what merging the transpositions of a rule at a time, and deduplicating each time, amounts to,
        % in a single pass rather than merging and deduplicating the lists once for each rule (cubic in the number of
        % rules).
        transpose(Rules, TempCtrRules, CtrRules) :-
            reverse(Rules, Reversed),
            findall(TrH, (member(H, Reversed), transposition(H, TrH)), Tran),
            utils::appendFast(TempCtrRules, Tran, All),
            utils::uniqueByFirst(All, CtrRules).

        transposition([Id, Prec, Effect], [Id, Prec, Effect]).
        transposition([Id, Prec, Effect], [NewId, NewPrec ,XNegated]) :-
            compound(Prec),
            parser::tupleToList(Prec, LPrec),!,
            transpositionSequential(LPrec, LPrec, Effect, Id, [], NewPrec, XNegated, NewId).
        transposition([Id, Prec, Effect], [NewId, EffectNegated ,XNegated]) :-
            \+ compound(Prec),
            Prec \== [],
            prologEscape(Prec),
            negate(Prec, XNegated),
            negate(Effect, EffectNegated),
            atom_concat(Id, '_i', NewId).

        transpositionSequential(LPrec, [H|T], Effect, Id, Skipped, NewPrec, XNegated, NewId) :-
            transpositionSequential(LPrec, T, Effect, Id, [H|Skipped], NewPrec, XNegated, NewId).
        transpositionSequential(LPrec, [X|T], Effect, Id, Skipped, NewPrec, XNegated, NewId) :-
            prologEscape(X),
            utils::appendFast(Skipped, T, CleanedPrec),
            negate(X, XNegated),
            negate(Effect, EffectNegated),
            listToTuple([EffectNegated|CleanedPrec], NewPrec),
            newIdentifier(Skipped, Id, NewId).

        newIdentifier(List, OldId, NewId) :-
            modifier(List, Mod),
            atom_concat(OldId, Mod, NewId).

        modifier([], '_i').
        modifier([_|Tail], Index):-
            modifier(Tail, Index1),
            atom_concat(Index1, 'i', Index).

        % The negation of a statement: -X of X, X of -X, and the obligations of permissions (o(-X) of p(X), o(X) of
        % p(-X)); the terms are matched as they are, not unified
        negate(X, Arg) :- nonvar(X), X = -(A), !, Arg = A.
        negate(X, Arg) :- nonvar(X), X = p(PX), !,
            (nonvar(PX), PX = -(PNX) -> Arg = o(PNX) ; Arg = o(-(PX))).
        negate(X, -(X)).

        prologEscape(X) :- \+ functor(X, 'prolog', _), \+ functor(X, '~', _).

        listToTuple([H], (H)) :- !.
        listToTuple([H|T], (H,TT)) :- listToTuple(T,TT).
        """.trimIndent()
}
