package it.unibo.tuprolog.argumentation.core.modules.language

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
            utils::append_fast(TempCtrRules, Tran, All),
            utils::unique_by_first(All, CtrRules).

        transposition([Id, Prec, Effect], [Id, Prec, Effect]).
        transposition([Id, Prec, Effect], [NewId, NewPrec ,XNegated]) :-
            compound(Prec),
            parser::tuple_to_list(Prec, LPrec),!,
            transposition_sequential(LPrec, LPrec, Effect, Id, [], NewPrec, XNegated, NewId).
        transposition([Id, Prec, Effect], [NewId, EffectNegated ,XNegated]) :-
            \+ compound(Prec),
            Prec \== [],
            prologEscape(Prec),
            negate(Prec, XNegated),
            negate(Effect, EffectNegated),
            atom_concat(Id, '_i', NewId).

        transposition_sequential(LPrec, [H|T], Effect, Id, Skipped, NewPrec, XNegated, NewId) :-
            transposition_sequential(LPrec, T, Effect, Id, [H|Skipped], NewPrec, XNegated, NewId).
        transposition_sequential(LPrec, [X|T], Effect, Id, Skipped, NewPrec, XNegated, NewId) :-
            prologEscape(X),
            utils::append_fast(Skipped, T, CleanedPrec),
            negate(X, XNegated),
            negate(Effect, EffectNegated),
            list_to_tuple([EffectNegated|CleanedPrec], NewPrec),
            newIdentifier(Skipped, Id, NewId).

        newIdentifier(List, OldId, NewId) :-
            modifier(List, Mod),
            atom_concat(OldId, Mod, NewId).

        modifier([], '_i').
        modifier([_|Tail], Index):-
            modifier(Tail, Index1),
            atom_concat(Index1, 'i', Index).

        negate(X, Arg) :-
            functor(X, '-', _) -> (
                arg(1, X, Arg));
            functor(X, 'p', _) -> (
                arg(1, X, PX),
                (functor(PX, '-', _) -> (
                    arg(1, PX, PNX),
                    Arg = o(PNX)
                );
                Arg = o(-(PX))));
            Arg = -(X).

        prologEscape(X) :- \+ functor(X, 'prolog', _), \+ functor(X, '~', _).

        list_to_tuple([H], (H)) :- !.
        list_to_tuple([H|T], (H,TT)) :- list_to_tuple(T,TT).
        """.trimIndent()
}
