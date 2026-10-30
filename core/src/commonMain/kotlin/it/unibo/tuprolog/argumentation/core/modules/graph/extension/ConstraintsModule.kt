package it.unibo.tuprolog.argumentation.core.modules.graph.extension

import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.graph.extension.Abstraction
import it.unibo.tuprolog.argumentation.core.primitives.graph.extension.RegionContains
import it.unibo.tuprolog.argumentation.core.primitives.graph.extension.RegionOverlaps
import it.unibo.tuprolog.argumentation.core.primitives.graph.extension.RegionReplace
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * Statements over regions of numbers (module `constraints`, a graphExtension): a statement may hold for a region in
 * place of a number, `from(T)` (after T), `until(T)` (up to T included), `between(A, B)` (after A, up to B included),
 * as `holdsAt(dead, from(2))` holds for every time after 2. A region stands for each of its numbers, with no argument
 * built for each of them:
 * - an argument for a region supports a statement for a number or a region within it through its restriction: the
 *   same argument concluding the narrower statement, built when a premise or a query needs it. The sub-arguments the
 *   region came from are restricted as well, so that the restriction rests on what holds at the number alone;
 * - two statements whose regions overlap conflict as they would for a number in common (`holdsAt(f, from(1))` and
 *   `-holdsAt(f, 7)`): the restrictions to the numbers outside the overlap do not conflict.
 *
 * The regions are where a statement has a region term; a number against a number is compared as any term.
 */
object ConstraintsModule : PrologModule("prolog.argumentation.graph.constraints", "constraints") {
    override val primitives: Map<Signature, Primitive>
        get() =
            listOf(RegionContains, RegionOverlaps, Abstraction.REGIONS, Abstraction.VALUES, RegionReplace)
                .associateBy { it.signature }

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! modifyArgumentationGraph
        %  Nothing to change: the restrictions are built with the arguments, the conflicts of the regions found with the
        %  attacks.
        modifyArgumentationGraph.

        %! active
        %  The extension is in effect.
        active :- parser::regions, !.

        %! restriction(+Tag, +Statement, -Restriction)
        %  Restriction, saved under Tag, is an argument for Statement restricting a saved argument for a region: where
        %  the conclusion of that argument has a region, Statement has a number or a region within it, and elsewhere the
        %  same terms.
        restriction(Tag, Statement, Restriction) :-
            valuesAbstract(Statement, Abstract, Pairs),
            Pairs \== [],
            graphStore::argumentFor([Abstract], Argument),
            within(Pairs, [], Map),
            Map \== [],
            restrict(Tag, Argument, Statement, Map, Restriction).

        % within(+Pairs, +Map, -Out): each value of Pairs (Term-Value, Term the one of the argument there) is the term,
        % or within its region; Out adds to Map the Region-Value pairs the argument is restricted by
        within([], Map, Map).
        within([Term-Value|Pairs], Map, Out) :-
            Term == Value, !,
            within(Pairs, Map, Out).
        within([Term-Value|Pairs], Map, Out) :-
            nonvar(Term),
            regionContains(Term, Value),
            within(Pairs, [Term-Value|Map], Out).

        % restrict(+Tag, +Argument, +Statement, +Map, -Restriction): Argument concluding Statement, its regions
        % replaced as Map says, saved with its sub-arguments restricted alike
        restrict(Tag, Argument, Statement, Map, Restriction) :-
            argument::rules(Argument, Rules),
            argument::topRule(Argument, TopRule),
            argument::body(Argument, Body),
            argument::info(Argument, Info),
            regionReplace(Body, Map, RestrictedBody),
            argument::new(Rules, TopRule, [Statement], RestrictedBody, Info, Restriction),
            (graphStore::argumentFor([Statement], Restriction) ->
                true
            ;
                graphStore::saveArgument(Tag, [Statement], Restriction),
                findall(Support, graphStore::support(Support, Argument), Supports),
                restrictSupports(Supports, Tag, Map, Restriction)
            ).

        % The supports of the restriction: those of the argument, restricted where the region came from them
        restrictSupports([], _, _, _).
        restrictSupports([Support|Supports], Tag, Map, Restriction) :-
            argument::claim(Support, Claim),
            regionReplace(Claim, Map, RestrictedClaim),
            (RestrictedClaim == Claim ->
                Restricted = Support
            ;
                restrict(Tag, Support, RestrictedClaim, Map, Restricted)
            ),
            graphStore::saveSupport(Restricted, Restriction),
            restrictSupports(Supports, Tag, Map, Restriction).

        %! buildConflictRestrictions
        %  For each saved argument, the restrictions of the regions to the statements in conflict with its conclusion: the
        %  conflict on a number shows in the graph with the region restricted to it (holdsAt(f, 7) for -holdsAt(f, 7)).
        buildConflictRestrictions :-
            findall(_, (
                graphStore::argument(Argument),
                argument::conclusion(Argument, Conclusion),
                conflicting(Conclusion, [Statement]),
                restriction(restriction, Statement, _)
            ), _).

        %! triggered(+Premises, +Conclusions)
        %  A premise among Premises has a number or a region within the region of one of Conclusions (new ones): the
        %  rule may use its restriction.
        triggered(Premises, Conclusions) :-
            member(Premise, Premises),
            valuesAbstract(Premise, Abstract, Pairs),
            Pairs \== [],
            member(Conclusion, Conclusions),
            \+ \+ (Abstract = Conclusion, within(Pairs, [], [_|_])), !.

        %! overlapConflict(+Statement, +Other)
        %  Statement conflicts with Other once their regions are set aside, and their regions overlap.
        overlapConflict([Statement], [Other]) :-
            regionsAbstract(Statement, AbstractStatement, StatementPairs),
            regionsAbstract(Other, AbstractOther, OtherPairs),
            (StatementPairs \== [] ; OtherPairs \== []), !,
            conflict::between([AbstractStatement], [AbstractOther]),
            overlapping(StatementPairs, OtherPairs),
            overlapping(OtherPairs, StatementPairs).

        % Each region of Pairs overlaps what the other statement has there: its region, a number, or a variable
        overlapping([], _).
        overlapping([Term-Region|Pairs], OtherPairs) :-
            overlaps(Term, Region, OtherPairs),
            overlapping(Pairs, OtherPairs).

        overlaps(Term, Region, OtherPairs) :-
            var(Term), !,
            (member(Var-OtherRegion, OtherPairs), Var == Term -> regionOverlaps(Region, OtherRegion) ; true).
        overlaps(Term, Region, _) :-
            regionOverlaps(Region, Term).

        %! generalized(+Statement, -General)
        %  General is Statement with its numbers and regions left open, when it has any.
        generalized(Statement, General) :-
            valuesAbstract(Statement, General, Pairs),
            Pairs \== [].

        %! queryArgument(+Statement, -Restriction)
        %  In query mode, a restriction for Statement, once the statement generalising it is explored.
        queryArgument(Statement, Restriction) :-
            generalized(Statement, General),
            queryExploration::exploreGeneral(General),
            restriction(query, Statement, Restriction).

        %! queryDependencies(+Argument, -Statement)
        %  The arguments for Statement may conflict with Argument through a region: the statements in conflict with its
        %  conclusion, its numbers and regions left open.
        queryDependencies(Argument, Statement) :-
            argument::conclusion(Argument, [Claim]),
            generalized(Claim, General),
            conflicting([General], [Statement]).

        %! queryDependants(+Argument, -Statement)
        %  The arguments for Statement may be attacked by Argument through a region (see queryDependencies/2).
        queryDependants(Argument, Statement) :-
            queryDependencies(Argument, Statement).

        conflicting(Conclusion, Statement) :- conflict::guarded(Statement, Conclusion, _).
        conflicting(Conclusion, Statement) :- conflict::unguarded(Statement, Conclusion).
        """.trimIndent()
}
