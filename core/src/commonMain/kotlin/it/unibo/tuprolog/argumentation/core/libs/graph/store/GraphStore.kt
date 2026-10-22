package it.unibo.tuprolog.argumentation.core.libs.graph.store

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

/**
 * The argumentation graph kept in the active context (module `graphStore`), saved and changed through it.
 *
 * Besides the graph itself (`argument/1`, `support/2`, `attack(Type, Attacker, Attacked, On)`), the context keeps:
 * - `conc(Conclusion) :- argument(A)` and `arg(Id) :- argument(A)`, the arguments by conclusion and by identifier
 *   (`context_id`), looked up with `context_check(clause(conc(C), argument(A)))`;
 * - `newConc(Tag, Conclusion)`, the conclusions of the arguments saved under a tag (the round of the graph builder);
 * - a compact copy of the graph by argument identifiers: `argInfo(Id, InfoId, Info)`, `supportId(IdA, IdB)`,
 *   `attackId(Type, IdA, IdB, IdOn)` and `att(IdA, IdB) :- attack(...)`. Loops over the graph go through it, so that
 *   resolution steps carry integers rather than whole arguments; InfoId identifies the rules and premises the
 *   preferences weigh (see the module argument).
 */
object GraphStore : PrologModule("prolog.argumentation.graph.store", "graphStore") {
    override val prologRawTheory: String =
        """
        %! saveArgument(+Tag, +Conclusion, +Argument)
        %  Argument, for Conclusion, is saved under Tag, unless it already is.
        saveArgument(Tag, Conclusion, Argument) :-
            context_absent(clause(conc(Conclusion), argument(Argument))),
            context_assert(newConc(Tag, Conclusion)),
            context_id(argument(Argument), Id),
            context_assert(argument(Argument)),
            context_assert(conc(Conclusion) :- argument(Argument)),
            context_assert(arg(Id) :- argument(Argument)),
            argument::info(Argument, Info),
            context_id(info(Info), InfoId),
            context_assert(argInfo(Id, InfoId, Info)).

        %! saveSupport(+Support, +Argument)
        %  Support, a sub-argument of Argument, is saved as supporting it, unless it already is.
        saveSupport(Support, Argument) :-
            context_absent(support(Support, Argument)),
            context_assert(support(Support, Argument)),
            context_id(argument(Support), IdSupport),
            context_id(argument(Argument), IdArgument),
            context_assert(supportId(IdSupport, IdArgument)).

        %! saveAttack(+Type, +Attacker, +Attacked, +On)
        %  Attacker attacks Attacked on its sub-argument On (Attacked itself for a direct attack), with an attack of Type.
        saveAttack(Type, Attacker, Attacked, On) :-
            context_id(argument(Attacker), IdAttacker),
            context_id(argument(Attacked), IdAttacked),
            context_id(argument(On), IdOn),
            saveAttackById(attackId(Type, IdAttacker, IdAttacked, IdOn), attack(Type, Attacker, Attacked, On)).

        %! saveAttackById(+Key, +Attack)
        %  The same attack, by identifiers (Key, attackId/4) and by arguments (Attack, attack/4), saved unless it
        %  already is.
        saveAttackById(Key, Attack) :-
            context_absent(Key),
            context_assert(Key),
            context_assert(Attack),
            Key = attackId(_, IdAttacker, IdAttacked, _),
            context_assert(att(IdAttacker, IdAttacked) :- Attack).

        %! removeAttack(+Type, +Attacker, +Attacked, +On)
        %  The attack is removed, in both its forms.
        removeAttack(Type, Attacker, Attacked, On) :-
            context_retract(attack(Type, Attacker, Attacked, On)),
            context_id(argument(Attacker), IdAttacker),
            context_id(argument(Attacked), IdAttacked),
            context_id(argument(On), IdOn),
            context_retract(attackId(Type, IdAttacker, IdAttacked, IdOn)),
            context_retract(att(IdAttacker, IdAttacked) :- attack(Type, Attacker, Attacked, On)).

        %! argumentById(?Id, ?Argument)
        %  Argument is saved with the identifier Id.
        argumentById(Id, Argument) :-
            context_check(clause(arg(Id), argument(Argument))).

        %! argumentFor(?Conclusion, ?Argument)
        %  Argument is saved for Conclusion.
        argumentFor(Conclusion, Argument) :-
            context_check(clause(conc(Conclusion), argument(Argument))).
        """.trimIndent()
}
