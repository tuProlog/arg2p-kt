package it.unibo.tuprolog.argumentation.core.modules.graph.partial

import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.graph.partial.GroundVars
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The part of the argumentation graph a query depends on (module `queryExploration`, for query mode): the arguments
 * for the query, and, recursively, those their labels may depend on, saved as they are built (see the module
 * argumentSearch).
 */
object QueryExplorationModule : PrologModule("prolog.argumentation.structured.exploration", "queryExploration") {
    override val primitives: Map<Signature, Primitive>
        get() = mapOf(GroundVars.descriptionPair)

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! explore(+Query)
        %  The arguments for Query and those their labels may depend on (see exploreStatement/2). With cycles among the
        %  rules, a statement needed as a premise while its arguments are being built only gets those built so far: the
        %  exploration is then repeated, from scratch, until it builds no new argument (the arguments are finitely many,
        %  as none uses a rule twice).
        explore(Query) :-
            graphStore::forgetNewConclusions(query),
            context_retract(cycle),
            exploreStatement(Query, dependency),
            (context_check(cycle), graphStore::newConclusion(query, _) ->
                forgetExplorations,
                explore(Query)
            ;
                true
            ).

        % A solver given arguments built elsewhere (as a worker of the parallel solver, which has some of the rules) carries
        % on its exploration as they come: resume/1 explores again what they may support, restart/0 forgets it all.

        %! resume(+Statements)
        %  The exploration carried on once arguments for Statements are saved: the statements explored whose rules use
        %  them as premises, directly or through other statements, explored again, the others left as they are.
        resume(Statements) :-
            builtOn(Statements, [], Affected),
            findall(_, (member(S, Affected), forgetExplored(S)), _),
            findall(_, (member(S, Affected), explore(S)), _).

        %! restart
        %  The exploration forgotten, not the arguments it built: exploring again finds all that the arguments saved
        %  since support or make possible attackers (with metaConflicts, those for conflicts).
        restart :-
            forgetExplorations,
            context_retract(expanded(_)),
            attackCandidates::forgetPossibleAttackers.

        % builtOn(+Statements, +Seen, -Affected): the statements explored whose rules use one of Statements as a premise,
        % as they were explored (q(X), rather than the instance q(1) of the rule using p(1)), and in turn those whose
        % rules use them; Seen are those found so far (with groundVars/2)
        builtOn([], _, []).
        builtOn([Statement|Statements], Seen, Affected) :-
            findall(S, (parser::ruleUsing(Statement, C), exploredAs(C, S)), Found),
            unseen(Found, Seen, New, Seen1),
            utils::appendLists([Statements, New], Next),
            builtOn(Next, Seen1, Rest),
            utils::appendLists([New, Rest], Affected).

        % exploredAs(+Statement, -Explored): Explored is explored and unifies with Statement, as it was explored
        exploredAs(Statement, Explored) :-
            context_check(explored(Explored)),
            \+ \+ Explored = Statement.

        unseen([], Seen, [], Seen).
        unseen([S|Ss], Seen, New, Seen1) :-
            groundVars(S, N),
            (member(N, Seen) ->
                unseen(Ss, Seen, New, Seen1)
            ;
                New = [S|New1],
                unseen(Ss, [N|Seen], New1, Seen1)
            ).

        %! explored(?Statement)
        %  Statement is explored (with the statements it generalises).
        explored(Statement) :-
            context_check(explored(Statement)).

        %! premiseOfExplored(?Premise)
        %  Premise is one of the premises, to be supported by an argument, of a rule concluding a statement explored, and
        %  the statement makes it ground. The exploration supports the premises one at a time, the next once the one
        %  before is, with its bindings: a solver whose arguments for them are built elsewhere asks for the ground ones
        %  all at once, so that they are built side by side (the bindings of the others leave them as they are).
        premiseOfExplored(Premise) :-
            context_check(explored(Statement)),
            parser::classicRule(_, Premises, [Statement]),
            member(Premise, Premises),
            ground(Premise),
            parser::argued(Premise).

        %! exploreAll(+Queries)
        %  Each of Queries explored (see explore/1).
        exploreAll(Queries) :-
            findall(_, (member(Query, Queries), explore(Query)), _).

        %! buildSubGraph(+Query, ?Argument)
        %  Argument is each argument for Query, a premise, once explored: (constraints) the restrictions of the arguments
        %  for a region containing it as well.
        buildSubGraph(Query, Argument) :-
            exploreStatement(Query, premise),
            graphStore::argumentFor([Query], Argument).
        buildSubGraph(Query, Argument) :-
            parser::regions,
            constraints::queryArgument(Query, Argument).

        %! exploreGeneral(+Statement)
        %  Statement, which generalises a statement asked for (see the module constraints), explored as a premise.
        exploreGeneral(Statement) :-
            exploreStatement(Statement, premise).

        % exploreStatement(+Query, +Use): builds the arguments for Query, and those their labels may depend on (see
        % dependency/3), recursively, unless it is explored (once, or once more for each round of explore/1): an
        % exploration covers the statements Query generalises, or Query alone (see cover/2). A statement met again while it is being explored only
        % has the arguments built so far: needed as a premise (Use), that is a cycle; needed as a dependency, its
        % exploration under way builds them all.
        exploreStatement(Query, _) :-
            groundVars(Query, Normalized),
            \+ covered(Normalized), !,
            scope(Normalized, Scope),
            context_assert(explored(Query)),
            cover(Query, Normalized),
            context_assert(exploring(Normalized, Query)),
            findall(_, (
                queryArgument(Query, Scope, Argument, Supports),
                recordArgument(Query, Argument, Supports),
                graphStore::argumentId(Argument, Id),
                dependency(Id, Argument, Statement),
                exploreStatement(Statement, dependency)
            ), _),
            context_retract(exploring(Normalized, _)).
        exploreStatement(Query, premise) :-
            groundVars(Query, Normalized),
            context_check(exploring(_, Normalized)), !,
            (context_absent(cycle) -> context_assert(cycle) ; true).
        exploreStatement(_, _).

        % The arguments for Query: those its rules and premises make, and (constraints) the restrictions of those for a
        % region containing it
        queryArgument(Query, Scope, Argument, Supports) :-
            argumentSearch::findSingleArgument([Query], Scope, Argument, Supports).
        queryArgument(Query, _, Argument, []) :-
            parser::regions,
            constraints::queryArgument(Query, Argument).

        % A statement explored covers the statements it generalises when its exploration finds their arguments as well
        % (parser::complete/1): a rule checking a variable it leaves unbound decides the check without the binding a
        % more specific statement gives, so such an exploration covers only the statement itself (its variants).
        cover(Query, _) :- parser::complete(Query), !, context_assert(coversAll(Query)).
        cover(_, Normalized) :- context_assert(coversOnly(Normalized)).

        covered(Normalized) :- context_check(coversAll(Normalized)), !.
        covered(Normalized) :- context_check(coversOnly(Normalized)).

        % The rules to explore a statement with: once a statement generalising it is explored, which covered it but for
        % the rules needing bindings, only those (see argumentSearch::findSingleArgument/4)
        scope(Normalized, needing) :- context_check(explored(Normalized)), !.
        scope(_, all).

        forgetExplorations :-
            context_retract(explored(_)),
            context_retract(coversAll(_)),
            context_retract(coversOnly(_)).

        forgetExplored(Statement) :-
            context_retract(explored(Statement)),
            context_retract(coversAll(Statement)),
            groundVars(Statement, Normalized),
            context_retract(coversOnly(Normalized)).

        % The statements whose arguments the label of an argument may depend on, besides its sub-arguments (built with
        % it): those that make others conflict with it (metaConflicts), those of its possible attackers, and those the
        % graph extensions relate it to. The conflicts come first, as the possible attackers are only looked for once.
        dependency(_, Argument, S) :- attackCandidates::attackDependency(Argument, S).
        dependency(Id, Argument, S) :- attackCandidates::possibleAttackers(Id, Argument, [S]).
        dependency(_, Argument, S) :- setting(graphExtension, X), X::queryDependencies(Argument, S).

        %! expand(+Locality)
        %  For the connected locality, the arguments whose labels may depend on the ones built (see dependant/2), with
        %  those they depend on in turn, until none is new.
        expand(connected) :-
            findall(Id, (graphStore::argumentById(Id, _), context_absent(expanded(Id))), Ids),
            Ids \== [], !,
            findall(_, (
                member(Id, Ids),
                context_assert(expanded(Id)),
                graphStore::argumentById(Id, Argument),
                dependant(Argument, Statement),
                explore(Statement)
            ), _),
            expand(connected).
        expand(_).

        % The statements whose arguments may depend on an argument: those it may attack, those its conclusion makes
        % conflict (metaConflicts), those of its super-arguments, and those the graph extensions relate to it
        dependant(Argument, S) :- attackCandidates::possiblyAttacked(Argument, S).
        dependant(Argument, S) :- attackCandidates::attackDependant(Argument, S).
        dependant(Argument, S) :-
            argument::claim(Argument, Claim),
            parser::ruleUsing(Claim, S).
        dependant(Argument, S) :- setting(graphExtension, X), X::queryDependants(Argument, S).

        % recordArgument(+Query, +Argument, +Supports): Argument saved with its supports, unless it already was (built in
        % a previous round of explore/1, whose dependencies are explored again)
        recordArgument(Query, Argument, Supports) :-
            graphStore::saveArgument(query, [Query], Argument), !,
            saveSupports(Argument, Supports).
        recordArgument(_, _, _).

        saveSupports(_, []).
        saveSupports(Argument, [Support|Supports]) :-
            graphStore::saveSupport(Support, Argument),
            saveSupports(Argument, Supports).
        """.trimIndent()
}
