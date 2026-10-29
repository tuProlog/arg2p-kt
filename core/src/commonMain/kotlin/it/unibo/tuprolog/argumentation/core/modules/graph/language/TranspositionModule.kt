package it.unibo.tuprolog.argumentation.core.modules.graph.language

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The transpositions of the strict rules (module `transposition`), with autoTransposition: for a strict rule
 * `r : a, b -> c`, the rules concluding a contradictory of each premise from a contradictory of the conclusion and
 * the other premises (`r_i : -c, b -> -a`, `r_ii : -c, a -> -b`). The contradictories of a statement are its
 * negation, and the statements the theory declares in conflict with it both ways, with no guard
 * (`conflict([c], [d])` and `conflict([d], [c])`, the declared conflicts being part of the conflict model): the
 * transpositions through those are named transposed(Id, [ConclusionContradictory, PremiseContradictory]), Id the
 * name of the one through negations.
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
            contradictories(Prec, Effect, XNegated, EffectNegated, Kind),
            suffixed(Id, '_i', BaseId),
            named(Kind, BaseId, [EffectNegated, XNegated], NewId).

        transpositionSequential(LPrec, [H|T], Effect, Id, Skipped, NewPrec, XNegated, NewId) :-
            transpositionSequential(LPrec, T, Effect, Id, [H|Skipped], NewPrec, XNegated, NewId).
        transpositionSequential(LPrec, [X|T], Effect, Id, Skipped, NewPrec, XNegated, NewId) :-
            prologEscape(X),
            utils::appendFast(Skipped, T, CleanedPrec),
            contradictories(X, Effect, XNegated, EffectNegated, Kind),
            listToTuple([EffectNegated|CleanedPrec], NewPrec),
            newIdentifier(Skipped, Id, BaseId),
            named(Kind, BaseId, [EffectNegated, XNegated], NewId).

        % The contradictories of a premise and of the conclusion of a rule: their negations (negation), or a pair of
        % which one at least is declared in conflict both ways (declared)
        contradictories(X, Effect, XNegated, EffectNegated, negation) :-
            negate(X, XNegated),
            negate(Effect, EffectNegated).
        contradictories(X, Effect, XNegated, EffectNegated, declared) :-
            contradictory(X, XNegated),
            contradictory(Effect, EffectNegated),
            (bothWays(X, XNegated) ; bothWays(Effect, EffectNegated)).

        contradictory(X, Negated) :- negate(X, Negated).
        contradictory(X, Other) :- bothWays(X, Other).

        % X and Other declared in conflict both ways by the theory, with no guard (not with metaConflicts, where the
        % conflicts are those arguments conclude, nor without the declared conflicts in the conflict model)
        bothWays(X, Other) :-
            \+ setting(metaConflicts),
            setting(conflictModel, declared),
            catch(conflict([X], [Other]), error(existence_error(_, _), _), fail),
            catch(conflict([Other], [X]), error(existence_error(_, _), _), fail).

        % The name of a transposition: through negations, the one of the rule with its suffix
        named(negation, Id, _, Id).
        named(declared, Id, Contradictories, transposed(Id, Contradictories)).

        newIdentifier(List, OldId, NewId) :-
            modifier(List, Mod),
            suffixed(OldId, Mod, NewId).

        % The identifier of a transposition: the one of the rule with a suffix, transposition(Id, Suffix) for a rule
        % whose identifier is no atom (a rule with parameters, t(X, Y))
        suffixed(Id, Suffix, NewId) :- atom(Id), !, atom_concat(Id, Suffix, NewId).
        suffixed(Id, Suffix, transposition(Id, Suffix)).

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

        % The premises transposed: neither Prolog goals, contraries, nor the body true of a fact
        prologEscape(X) :- X \== true, \+ functor(X, 'prolog', _), \+ functor(X, '~', _).

        listToTuple([H], (H)) :- !.
        listToTuple([H|T], (H,TT)) :- listToTuple(T,TT).
        """.trimIndent()
}
