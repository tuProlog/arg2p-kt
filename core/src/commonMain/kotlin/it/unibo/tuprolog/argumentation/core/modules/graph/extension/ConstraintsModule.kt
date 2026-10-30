package it.unibo.tuprolog.argumentation.core.modules.graph.extension

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The arguments holding for sets of values (module `constraints`, a graphExtension, on with temporal as well): without
 * it no argument concludes an open statement (with free variables); with it one may, for the values its variables may
 * take (constrained by its `{ }` premises, see the module constraint): `holdsAt(f, T)` with `{ T > 1 }` holds for every
 * T after 1. Its instance for some of them is an argument of its own, made of the same rules, its sub-arguments
 * instantiated alike where the values flow:
 * - an argument using it for some values (`alarm(5)` from `holdsAt(f, 5)`) rests on its instance for them;
 * - a conflict binding its variables (`-holdsAt(f, 7)`) attacks it, and its instance for those values, which the graph
 *   shows.
 *
 * An argument attacked for some of its values is attacked for all of them (it states them all), and its instances for
 * the others are not: each instance stands or falls on its own values. Nothing is removed from the graph.
 */
object ConstraintsModule : PrologModule("prolog.argumentation.graph.constraints", "constraints") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! modifyArgumentationGraph
        %  Nothing to change: the instances are added with the arguments (see buildAdditionalArguments/0).
        modifyArgumentationGraph.

        %! buildAdditionalArguments
        %  The instances where a conflict binds the variables of an argument.
        buildAdditionalArguments :-
            graphStore::arguments(Arguments),
            findall(_, (member(A, Arguments), conflictInstance(A)), _).

        % The instances of A and of its possible attackers for the values a conflict between them binds
        conflictInstance(A) :-
            argument::claim(A, ClaimA),
            graphStore::argumentId(A, IdA),
            attackCandidates::possibleAttackers(IdA, A, [Head]),
            graphStore::argumentUnifiable([Head], B),
            argument::claim(B, ClaimB),
            \+ (ground(ClaimA), ground(ClaimB)),
            copy_term(B-A, CB-CA),
            attackRelation::attacks(_, CB, CA),
            constraint::compatible(CB, CA),
            realize(CA),
            realize(CB),
            fail.
        conflictInstance(_).

        %! realize(+Instance)
        %  Instance, a saved argument with bindings, saved unless it is (an instance for fewer values), with the supports
        %  of the argument bound alike: instances in turn where the values flow into them.
        realize(Instance) :-
            graphStore::argumentId(Instance, Id),
            graphStore::argumentById(Id, _), !.
        realize(Instance) :-
            general(Instance, General), !,
            graphStore::supportersOf(General, Supporters),
            argument::body(General, Body),
            argument::body(Instance, Bound),
            argument::conclusion(Instance, Conclusion),
            graphStore::saveInstance(instance, Conclusion, Instance),
            findall(_, (member(Sub, Supporters), realizeSupport(Sub, Body, Bound, Instance)), _).
        realize(_).

        % The saved argument Instance is an instance of
        general(Instance, General) :-
            argument::conclusion(Instance, Conclusion),
            graphStore::argumentUnifiable(Conclusion, General),
            constraint::subsumes(General, Instance).

        % Sub, supporting the premise of Body that Bound, the body of Instance, binds: its instance for that binding
        realizeSupport(Sub, Body, Bound, Instance) :-
            argument::claim(Sub, Claim),
            premiseAt(Body, Claim, Bound, Premise),
            copy_term(Sub, Instantiated),
            argument::claim(Instantiated, Premise),
            realize(Instantiated),
            graphStore::saveSupport(Instantiated, Instance).

        premiseAt([P|_], Claim, [B|_], B) :- \+ \+ P = Claim, !.
        premiseAt([_|Ps], Claim, [_|Bs], B) :- premiseAt(Ps, Claim, Bs, B).
        """.trimIndent()
}
