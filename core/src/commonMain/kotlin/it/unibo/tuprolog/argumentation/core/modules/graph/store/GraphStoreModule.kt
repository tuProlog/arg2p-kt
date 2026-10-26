package it.unibo.tuprolog.argumentation.core.modules.graph.store

import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.graph.ExpandedAttack
import it.unibo.tuprolog.argumentation.core.primitives.graph.ExpandedAttackById
import it.unibo.tuprolog.argumentation.core.primitives.graph.ExpandedAttackIn
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The argumentation graph kept in the active context, and its labels (module `graphStore`): the only module reading
 * and writing them in the context; the terms are those of the data modules (argument, support, attack, label,
 * statement).
 *
 * Besides the graph itself (`argument/1`, `support/2`, `attack/4` terms), the context keeps:
 * - `conc(Conclusion) :- argument(A)` and `arg(Id) :- argument(A)`, the arguments by conclusion and by identifier
 *   (`context_id`);
 * - `newConc(Tag, Conclusion)`, the conclusions of the arguments saved under a tag (the round of the graph builder);
 * - a compact copy of the graph by argument identifiers: `argInfo(Id, InfoId, Info)`, `supportId(IdA, IdB)`,
 *   `attackId(Type, IdA, IdB, IdOn)`. Loops over the graph go through it, so that resolution steps carry integers
 *   rather than whole arguments; InfoId identifies the info of the arguments (see the module argument), what the
 *   preferences weigh;
 * - the labels of the arguments and of the statements (see the module label).
 *
 * Only the attacks found are saved: an attack on an argument is an attack on every argument built on it as well, and
 * the attacks are read with those (attack/4, attackById/4, attacksById/2, attackIn/5, and the attack graph of the
 * native labellings), extended once until the attacks or the supports change (see ExpandedAttacks).
 */
object GraphStoreModule : PrologModule("prolog.argumentation.graph.store", "graphStore") {
    override val primitives: Map<Signature, Primitive>
        get() = listOf(ExpandedAttack, ExpandedAttackById, ExpandedAttackIn).associateBy { it.signature }

    override val prologRawTheory: String =
        // language=Prolog
        """
        %--------------------------------------------------------------------------
        % Arguments
        %--------------------------------------------------------------------------

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

        %! argument(?Argument)
        %  Argument is saved.
        argument(Argument) :-
            context_check(argument(Argument)).

        %! arguments(-Arguments)
        %  Arguments are the saved arguments.
        arguments(Arguments) :-
            findall(Argument, context_check(argument(Argument)), Arguments).

        %! argumentFor(?Conclusion, ?Argument)
        %  Argument is saved for Conclusion.
        argumentFor(Conclusion, Argument) :-
            context_check(clause(conc(Conclusion), argument(Argument))).

        %! argumentUnifiable(+Conclusion, -Argument)
        %  Argument is saved for a conclusion unifying with Conclusion, which it is not bound to.
        argumentUnifiable(Conclusion, Argument) :-
            context_unifiable(conc(Conclusion), argument(Argument)).

        %! argumentById(?Id, ?Argument)
        %  Argument is saved with the identifier Id.
        argumentById(Id, Argument) :-
            context_check(clause(arg(Id), argument(Argument))).

        %! argumentId(+Argument, -Id)
        %  Id is the identifier of Argument (see context_id/2).
        argumentId(Argument, Id) :-
            context_id(argument(Argument), Id).

        %! argumentIds(-Ids)
        %  Ids are the identifiers of the saved arguments.
        argumentIds(Ids) :-
            findall(Id, context_check(clause(arg(Id), _)), Ids).

        %! argumentByIdIn(+Branch, ?Id, ?Argument)
        %  Argument is saved with the identifier Id in the context branch Branch.
        argumentByIdIn(Branch, Id, Argument) :-
            context_check(Branch, clause(arg(Id), argument(Argument))).

        %! excludeArguments
        %  The saved arguments are left out of the graph the labellings label (by their identifiers), until included.
        excludeArguments :-
            context_retract(arg(_) :- argument(_)).

        %! includeArgument(+Id, +Argument)
        %  Argument, with identifier Id, is in the graph the labellings label again.
        includeArgument(Id, Argument) :-
            context_assert(arg(Id) :- argument(Argument)).

        %! argumentInfo(?Id, ?InfoId, ?Info)
        %  The argument with identifier Id has the info Info, with identifier InfoId.
        argumentInfo(Id, InfoId, Info) :-
            context_check(argInfo(Id, InfoId, Info)).

        %! newConclusion(?Tag, ?Conclusion)
        %  An argument for Conclusion was saved under Tag.
        newConclusion(Tag, Conclusion) :-
            context_check(newConc(Tag, Conclusion)).

        %! forgetNewConclusions(?Tag)
        %  The conclusions saved under Tag are forgotten (not their arguments).
        forgetNewConclusions(Tag) :-
            context_retract(newConc(Tag, _)).

        %--------------------------------------------------------------------------
        % Supports
        %--------------------------------------------------------------------------

        %! saveSupport(+Supporter, +Supported)
        %  Supporter, a sub-argument of Supported, is saved as supporting it, unless it already is.
        saveSupport(Supporter, Supported) :-
            support::new(Supporter, Supported, Support),
            context_absent(Support),
            context_assert(Support),
            context_id(argument(Supporter), IdSupporter),
            context_id(argument(Supported), IdSupported),
            context_assert(supportId(IdSupporter, IdSupported)).

        %! support(?Supporter, ?Supported)
        %  Supporter supports Supported.
        support(Supporter, Supported) :-
            support::new(Supporter, Supported, Support),
            context_check(Support).

        %! supports(-Supports)
        %  Supports are the (Supporter, Supported) pairs saved.
        supports(Supports) :-
            findall((Supporter, Supported), support(Supporter, Supported), Supports).

        %! supportedById(+Id, -SuperId)
        %  The argument with identifier SuperId is supported by the one with identifier Id, directly or not.
        supportedById(Id, SuperId) :-
            context_closure(supportId, Id, SuperId).

        %--------------------------------------------------------------------------
        % Attacks
        %--------------------------------------------------------------------------

        %! saveAttack(+Type, +Attacker, +Attacked, +On)
        %  Attacker attacks Attacked on its sub-argument On (Attacked itself for a direct attack), with an attack of
        %  Type, unless it already does.
        saveAttack(Type, Attacker, Attacked, On) :-
            context_id(argument(Attacker), IdAttacker),
            context_id(argument(Attacked), IdAttacked),
            context_id(argument(On), IdOn),
            saveAttackById(Type, IdAttacker, Attacker, IdAttacked, Attacked, IdOn, On).

        %! saveAttackById(+Type, +IdAttacker, +Attacker, +IdAttacked, +Attacked, +IdOn, +On)
        %  As saveAttack/4, with the identifiers of the arguments known.
        saveAttackById(Type, IdAttacker, Attacker, IdAttacked, Attacked, IdOn, On) :-
            context_absent(attackId(Type, IdAttacker, IdAttacked, IdOn)),
            context_assert(attackId(Type, IdAttacker, IdAttacked, IdOn)),
            attack::new(Type, Attacker, Attacked, On, Attack),
            context_assert(Attack).

        %! removeAttack(+Type, +Attacker, +Attacked, +On)
        %  The attack is removed, in all its forms.
        removeAttack(Type, Attacker, Attacked, On) :-
            attack::new(Type, Attacker, Attacked, On, Attack),
            context_retract(Attack),
            context_id(argument(Attacker), IdAttacker),
            context_id(argument(Attacked), IdAttacked),
            context_id(argument(On), IdOn),
            context_retract(attackId(Type, IdAttacker, IdAttacked, IdOn)).

        %! attack(?Type, ?Attacker, ?Attacked, ?On)
        %  Attacker attacks Attacked on On, with an attack of Type: saved, or extended to Attacked from On, which it is
        %  built on.
        attack(Type, Attacker, Attacked, On) :-
            expandedAttack(Type, Attacker, Attacked, On).

        %! attacks(-Attacks)
        %  Attacks are the (Type, Attacker, Attacked, On) attacks, saved or extended (see attack/4).
        attacks(Attacks) :-
            findall((Type, Attacker, Attacked, On), attack(Type, Attacker, Attacked, On), Attacks).

        %! attackById(?Type, ?IdAttacker, ?IdAttacked, ?IdOn)
        %  The attack, saved or extended, by the identifiers of its arguments.
        attackById(Type, IdAttacker, IdAttacked, IdOn) :-
            expandedAttackById(Type, IdAttacker, IdAttacked, IdOn).

        %! attacksById(?IdAttacker, ?IdAttacked)
        %  The argument with identifier IdAttacker attacks the one with identifier IdAttacked.
        attacksById(IdAttacker, IdAttacked) :-
            expandedAttackById(_, IdAttacker, IdAttacked, _).

        %--------------------------------------------------------------------------
        % Labels
        %--------------------------------------------------------------------------

        %! labelArgument(+Label, +Argument)
        %  Argument is labelled with Label (in, out or und).
        labelArgument(Label, Argument) :-
            label::argument(Label, Argument, Fact),
            context_assert(Fact).

        %! labelArgument(+Label, +Argument, +Id)
        %  Argument, with identifier Id, is labelled with Label, by itself and by its identifier.
        labelArgument(Label, Argument, Id) :-
            labelArgument(Label, Argument),
            label::identifier(Label, Id, Fact),
            context_assert(Fact).

        %! unlabelArguments(+Label)
        %  The arguments labelled with Label no longer are, by themselves and by their identifiers.
        unlabelArguments(Label) :-
            label::argument(Label, _, Fact),
            context_retract(Fact),
            label::identifier(Label, _, IdFact),
            context_retract(IdFact).

        %! argumentLabelById(?Label, ?Id)
        %  The argument with identifier Id is labelled with Label.
        argumentLabelById(Label, Id) :-
            label::identifier(Label, Id, Fact),
            context_check(Fact).

        %! argumentLabelByIdIn(+Branch, ?Label, ?Id)
        %  The argument with identifier Id is labelled with Label in the context branch Branch.
        argumentLabelByIdIn(Branch, Label, Id) :-
            label::identifier(Label, Id, Fact),
            context_check(Branch, Fact).

        %! labelledIds(+Label, -Ids)
        %  Ids are the identifiers of the arguments labelled with Label.
        labelledIds(Label, Ids) :-
            findall(Id, argumentLabelById(Label, Id), Ids).

        %! argumentLabel(?Label, ?Argument)
        %  Argument is labelled with Label.
        argumentLabel(Label, Argument) :-
            label::argument(Label, Argument, Fact),
            context_check(Fact).

        %! labelStatement(+Label, +Statement)
        %  Statement is labelled with Label.
        labelStatement(Label, Statement) :-
            label::statement(Label, Statement, Fact),
            context_assert(Fact).

        %! statementLabel(?Label, ?Statement)
        %  Statement is labelled with Label.
        statementLabel(Label, Statement) :-
            label::statement(Label, Statement, Fact),
            context_check(Fact).

        %! labelledArguments(+Label, -Arguments)
        %  Arguments are those labelled with Label.
        labelledArguments(Label, Arguments) :-
            findall(Argument, argumentLabel(Label, Argument), Arguments).

        %--------------------------------------------------------------------------
        % The graph and its labels in a given context branch, rather than the active one
        %--------------------------------------------------------------------------

        %! argumentIn(+Branch, ?Argument)
        argumentIn(Branch, Argument) :-
            context_check(Branch, argument(Argument)).

        %! supportIn(+Branch, ?Supporter, ?Supported)
        supportIn(Branch, Supporter, Supported) :-
            support::new(Supporter, Supported, Support),
            context_check(Branch, Support).

        %! attackIn(+Branch, ?Type, ?Attacker, ?Attacked, ?On)
        attackIn(Branch, Type, Attacker, Attacked, On) :-
            expandedAttackIn(Branch, Type, Attacker, Attacked, On).

        %! argumentLabelIn(+Branch, ?Label, ?Argument)
        argumentLabelIn(Branch, Label, Argument) :-
            label::argument(Label, Argument, Fact),
            context_check(Branch, Fact).

        %--------------------------------------------------------------------------
        % The whole graph and its labels, sorted
        %--------------------------------------------------------------------------

        %! graph(-Arguments, -Attacks, -Supports)
        %  The arguments, the (Type, A, B, C) attacks and the (A, B) supports, each sorted.
        graph(Arguments, Attacks, Supports) :-
            arguments(AllArguments),
            attacks(AllAttacks),
            supports(AllSupports),
            utils::sort(AllArguments, Arguments),
            utils::sort(AllAttacks, Attacks),
            utils::sort(AllSupports, Supports).

        %! argumentLabelling(-In, -Out, -Und)
        %  The arguments labelled IN, OUT and UND, each sorted.
        argumentLabelling(In, Out, Und) :-
            sortedLabelled(argumentLabel, in, In),
            sortedLabelled(argumentLabel, out, Out),
            sortedLabelled(argumentLabel, und, Und).

        %! statementLabelling(-In, -Out, -Und)
        %  The statements labelled IN, OUT and UND, each sorted.
        statementLabelling(In, Out, Und) :-
            sortedLabelled(statementLabel, in, In),
            sortedLabelled(statementLabel, out, Out),
            sortedLabelled(statementLabel, und, Und).

        %! claimLabelling(-In, -Out, -Und)
        %  The claims of the arguments labelled IN, OUT and UND (the abstract arguments, see abstract::solve/5), each sorted.
        claimLabelling(In, Out, Und) :-
            sortedLabelled(claimLabel, in, In),
            sortedLabelled(claimLabel, out, Out),
            sortedLabelled(claimLabel, und, Und).

        sortedLabelled(Kind, Label, Sorted) :-
            findall(X, labelledAs(Kind, Label, X), All),
            utils::sort(All, Sorted).

        labelledAs(argumentLabel, Label, Argument) :- argumentLabel(Label, Argument).
        labelledAs(statementLabel, Label, Statement) :- statementLabel(Label, Statement).
        labelledAs(claimLabel, Label, Claim) :- argument::claim(Argument, Claim), argumentLabel(Label, Argument).
        """.trimIndent()
}
