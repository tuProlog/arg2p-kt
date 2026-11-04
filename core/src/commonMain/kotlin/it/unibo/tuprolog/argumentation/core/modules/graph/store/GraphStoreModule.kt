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
 * An argument is kept once, as the term of its identifier (`context_id`, `context_term`): `argument/1` holds that record,
 * looked up by the claim inside it, and the rest of the graph refers to it by the identifier. The context keeps:
 * - `arg(Id)`, the arguments of the graph the labellings label;
 * - `newConc(Tag, Conclusion)`, the conclusions of the arguments saved under a tag (the round of the graph builder);
 * - the graph by these identifiers: `argInfo(Id, InfoId)`, `supportId(IdA, IdB)`, `attackId(Type, IdA, IdB, IdOn)`; InfoId
 *   identifies the info of the argument (see the module argument), what the preferences weigh;
 * - the labels of the arguments and of the statements (see the module label): `in(A)` shares the record of `argument(A)`.
 *
 * `support/2` and `attack/4`, looked up in the context, are answered from the identifiers, each argument read on its own.
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

        %! saveArgument(+Conclusion, +Argument)
        %  Argument, whose conclusion is Conclusion, is saved, unless it already is.
        saveArgument(Conclusion, Argument) :-
            context_absent(argument(Argument)),
            argument::conclusion(Argument, Conclusion),
            context_id(argument(Argument), Id),
            store(Argument, Id).

        %! saveArgument(+Tag, +Conclusion, +Argument)
        %  As saveArgument/2, Conclusion noted as new under Tag (see newConclusion/2): the rounds of the graph builder.
        saveArgument(Tag, Conclusion, Argument) :-
            saveArgument(Conclusion, Argument),
            context_assert(newConc(Tag, Conclusion)).

        %! saveInstance(+Conclusion, +Argument)
        %  As saveArgument/2, unless an argument with the identifier of Argument is saved: Argument is saved even when an
        %  argument it is an instance of (for more values, see the module constraints) is.
        saveInstance(Conclusion, Argument) :-
            context_id(argument(Argument), Id),
            context_absent(arg(Id)),
            argument::conclusion(Argument, Conclusion),
            store(Argument, Id).

        store(Argument, Id) :-
            context_assert_id(Id),
            context_assert(arg(Id)),
            argument::info(Argument, Info),
            context_id(info(Info), InfoId),
            context_assert(argInfo(Id, InfoId)).

        %! saveArguments(+Tag, +Arguments)
        %  Each of Arguments saved under Tag, for its conclusion (see saveArgument/3).
        saveArguments(Tag, Arguments) :-
            findall(_, (
                member(Argument, Arguments),
                argument::conclusion(Argument, Conclusion),
                (saveArgument(Tag, Conclusion, Argument) -> true ; true)
            ), _).

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
            context_by(conclusion, Conclusion, argument(Argument)).

        %! argumentUnifiable(+Conclusion, -Argument)
        %  Argument is saved for a conclusion unifying with Conclusion, which it is not bound to.
        argumentUnifiable(Conclusion, Argument) :-
            context_matching(conclusion, Conclusion, argument(Argument)).

        %! argumentById(?Id, ?Argument)
        %  Argument is saved with the identifier Id.
        argumentById(Id, Argument) :-
            context_check(arg(Id)),
            context_term(Id, argument(Argument)).

        %! argumentId(+Argument, -Id)
        %  Id is the identifier of Argument (see context_id/2).
        argumentId(Argument, Id) :-
            context_id(argument(Argument), Id).

        %! argumentIds(-Ids)
        %  Ids are the identifiers of the saved arguments.
        argumentIds(Ids) :-
            findall(Id, context_check(arg(Id)), Ids).

        %! argumentByIdIn(+Branch, ?Id, ?Argument)
        %  Argument is saved with the identifier Id in the context branch Branch.
        argumentByIdIn(Branch, Id, Argument) :-
            context_check(Branch, arg(Id)),
            context_term(Id, argument(Argument)).

        %! excludeArguments
        %  The saved arguments are left out of the graph the labellings label (by their identifiers), until included.
        excludeArguments :-
            context_retract(arg(_)).

        %! includeArgument(+Id, +Argument)
        %  Argument, with identifier Id, is in the graph the labellings label again.
        includeArgument(Id, _) :-
            context_assert(arg(Id)).

        %! argumentInfo(?Id, ?InfoId, ?Info)
        %  The argument with identifier Id has the info Info, with identifier InfoId.
        argumentInfo(Id, InfoId, Info) :-
            context_check(argInfo(Id, InfoId)),
            context_term(InfoId, info(Info)).

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
        %  Supporter, a sub-argument of Supported, is saved as supporting it, unless it already is: by their identifiers,
        %  as the support of an instance of Supported by one of Supporter (for fewer values) is another one.
        saveSupport(Supporter, Supported) :-
            context_id(argument(Supporter), IdSupporter),
            context_id(argument(Supported), IdSupported),
            context_absent(supportId(IdSupporter, IdSupported)),
            context_assert(supportId(IdSupporter, IdSupported)).

        %! saveSupports(+Supports)
        %  Each of Supports, support(Supporter, Supported), saved (see saveSupport/2).
        saveSupports(Supports) :-
            findall(_, (member(support(Supporter, Supported), Supports), (saveSupport(Supporter, Supported) -> true ; true)), _).

        %! saveSupporters(+Argument, +Supporters)
        %  Each of Supporters saved as a support of Argument (see saveSupport/2).
        saveSupporters(Argument, Supporters) :-
            findall(_, (member(Supporter, Supporters), saveSupport(Supporter, Argument)), _).

        %! support(?Supporter, ?Supported)
        %  Supporter supports Supported.
        support(Supporter, Supported) :-
            nonvar(Supported), !,
            context_stored_id(argument(Supported), IdSupported),
            context_check(supportId(IdSupporter, IdSupported)),
            supportOf(IdSupporter, Supporter, IdSupported, Supported).
        support(Supporter, Supported) :-
            nonvar(Supporter), !,
            context_stored_id(argument(Supporter), IdSupporter),
            context_check(supportId(IdSupporter, IdSupported)),
            supportOf(IdSupporter, Supporter, IdSupported, Supported).
        support(Supporter, Supported) :-
            context_check(supportId(IdSupporter, IdSupported)),
            supportOf(IdSupporter, Supporter, IdSupported, Supported).

        supportOf(IdSupporter, Supporter, IdSupported, Supported) :-
            context_term(IdSupporter, argument(Supporter)),
            context_term(IdSupported, argument(Supported)).

        %! supports(-Supports)
        %  Supports are the (Supporter, Supported) pairs saved.
        supports(Supports) :-
            findall((Supporter, Supported), support(Supporter, Supported), Supports).

        %! supportersOf(+Argument, -Supporters)
        %  Supporters are the saved arguments supporting Argument directly, by their identifiers.
        supportersOf(Argument, Supporters) :-
            context_id(argument(Argument), Id),
            findall(S, (context_check(supportId(IdS, Id)), argumentById(IdS, S)), Supporters).

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

        %! saveAttacks(+Attacks)
        %  Each of Attacks, attack(Type, Attacker, Attacked, On), saved (see saveAttack/4).
        saveAttacks(Attacks) :-
            findall(_, (member(attack(Type, Attacker, Attacked, On), Attacks), (saveAttack(Type, Attacker, Attacked, On) -> true ; true)), _).

        %! saveAttackById(+Type, +IdAttacker, +Attacker, +IdAttacked, +Attacked, +IdOn, +On)
        %  As saveAttack/4, with the identifiers of the arguments known.
        saveAttackById(Type, IdAttacker, Attacker, IdAttacked, Attacked, IdOn, On) :-
            context_absent(attackId(Type, IdAttacker, IdAttacked, IdOn)),
            context_assert(attackId(Type, IdAttacker, IdAttacked, IdOn)).

        %! removeAttack(+Type, +Attacker, +Attacked, +On)
        %  The attack, saved (see saveAttack/4), is removed in all its forms (those extended from it with it); nothing
        %  is done for an attack not saved (an extended one, say).
        removeAttack(Type, Attacker, Attacked, On) :-
            context_id(argument(Attacker), IdAttacker),
            context_id(argument(Attacked), IdAttacked),
            context_id(argument(On), IdOn),
            context_retract(attackId(Type, IdAttacker, IdAttacked, IdOn)).

        %! attackFound(?Type, ?Attacker, ?Attacked, ?On)
        %  The attack is saved, as it was found (attack/4 extends it): what context_check(attack(Type, Attacker, Attacked, On))
        %  answers, as context_check(support(Supporter, Supported)) support/2.
        attackFound(Type, Attacker, Attacked, On) :-
            context_check(attackId(Type, IdAttacker, IdAttacked, IdOn)),
            attackOf(IdAttacker, Attacker, IdAttacked, Attacked, IdOn, On).

        attackOf(IdAttacker, Attacker, IdAttacked, Attacked, IdOn, On) :-
            context_term(IdAttacker, argument(Attacker)),
            context_term(IdAttacked, argument(Attacked)),
            context_term(IdOn, argument(On)).

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

        %! directAttack(?Type, ?Attacker, ?Attacked)
        %  Attacker attacks Attacked itself, with an attack of Type: an attack saved, which attack/4 extends to the
        %  arguments built on Attacked. The steps changing the attacks read these: the extended ones follow. (The
        %  arguments are given as they are saved, as attack/4 gives them: removing an attack identifies them, see
        %  context_id.)
        directAttack(Type, Attacker, Attacked) :-
            expandedAttack(Type, Attacker, Attacked, On),
            Attacked == On.

        %! directAttacks(-Attacks)
        %  Attacks are the (Type, Attacker, Attacked) attacks saved (see directAttack/3).
        directAttacks(Attacks) :-
            findall((Type, Attacker, Attacked), directAttack(Type, Attacker, Attacked), Attacks).

        %! reaches(+A, +B)
        %  B is A or an argument reached from A through attacks: the arguments are visited breadth first, each once
        %  (attack cycles are common, rebuttals being mutual).
        reaches(A, B) :- reached([A], [], B), !.

        reached([B|_], _, B) :- !.
        reached([A|Frontier], Visited, B) :-
            findall(C, (attack(_, A, C, _), \+ member(C, [A|Visited]), \+ member(C, Frontier)), Next),
            append(Frontier, Next, Queue),
            reached(Queue, [A|Visited], B).

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
        %  Statement is labelled with Label, by the identifier of the label as well (see statementLabelled/2).
        labelStatement(Label, Statement) :-
            label::statement(Label, Statement, Fact),
            context_id(Fact, Key),
            context_assert_id(Key),
            context_assert(statementKey(Key)).

        %! statementLabel(?Label, ?Statement)
        %  Statement is labelled with Label.
        statementLabel(Label, Statement) :-
            label::statement(Label, Statement, Fact),
            context_check(Fact).

        %! statementLabelled(+Label, +Statement)
        %  Statement, as it is written (up to the names of its variables), is labelled with Label: unlike
        %  statementLabel/2, a statement with variables is not labelled by those of its instances labelled, nor they by
        %  it (see the module constraints).
        statementLabelled(Label, Statement) :-
            label::statement(Label, Statement, Fact),
            context_id(Fact, Key),
            context_check(statementKey(Key)).

        %! labelledArguments(+Label, -Arguments)
        %  Arguments are those labelled with Label.
        labelledArguments(Label, Arguments) :-
            findall(Argument, argumentLabel(Label, Argument), Arguments).

        %--------------------------------------------------------------------------
        % The graph and its labels in a given context branch, rather than the active one (read by Kotlin, see
        % GraphMining)
        %--------------------------------------------------------------------------

        %! argumentIn(+Branch, ?Argument)
        argumentIn(Branch, Argument) :-
            context_check(Branch, argument(Argument)).

        %! supportIn(+Branch, ?Supporter, ?Supported)
        supportIn(Branch, Supporter, Supported) :-
            context_check(Branch, supportId(IdSupporter, IdSupported)),
            supportOf(IdSupporter, Supporter, IdSupported, Supported).

        %! attackFoundIn(+Branch, ?Type, ?Attacker, ?Attacked, ?On)
        attackFoundIn(Branch, Type, Attacker, Attacked, On) :-
            context_check(Branch, attackId(Type, IdAttacker, IdAttacked, IdOn)),
            attackOf(IdAttacker, Attacker, IdAttacked, Attacked, IdOn, On).

        %! attackIn(+Branch, ?Type, ?Attacker, ?Attacked, ?On)
        attackIn(Branch, Type, Attacker, Attacked, On) :-
            expandedAttackIn(Branch, Type, Attacker, Attacked, On).

        %! argumentLabelIn(+Branch, ?Label, ?Argument)
        argumentLabelIn(Branch, Label, Argument) :-
            label::argument(Label, Argument, Fact),
            context_check(Branch, Fact).

        %--------------------------------------------------------------------------

        %--------------------------------------------------------------------------
        % The whole graph and its labels, sorted
        %--------------------------------------------------------------------------

        %! graph(-Arguments, -Attacks, -Supports)
        %  The arguments, the (Type, A, B, C) attacks and the (A, B) supports, each sorted.
        graph(Arguments, Attacks, Supports) :-
            arguments(AllArguments),
            attacks(AllAttacks),
            supports(AllSupports),
            utils::sortDescending(AllArguments, Arguments),
            utils::sortDescending(AllAttacks, Attacks),
            utils::sortDescending(AllSupports, Supports).

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
            utils::sortDescending(All, Sorted).

        labelledAs(argumentLabel, Label, Argument) :- argumentLabel(Label, Argument).
        labelledAs(statementLabel, Label, Statement) :- statementLabel(Label, Statement).
        labelledAs(claimLabel, Label, Claim) :- argument::claim(Argument, Claim), argumentLabel(Label, Argument).
        """.trimIndent()
}
