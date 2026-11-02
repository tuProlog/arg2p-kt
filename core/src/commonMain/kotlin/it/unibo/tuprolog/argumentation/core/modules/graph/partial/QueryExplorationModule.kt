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
                context_retract(explored(_)),
                explore(Query)
            ;
                true
            ).

        %! explore(+Query, +Locality)
        %  The part of the graph the labels of the arguments for Query depend on, as Locality goes (see
        %  structuredPartial::locality/1): the arguments for Query and those they depend on (explore/1), those depending
        %  on them for the connected locality (expand/1), then what the steps changing the graph relate to them
        %  (extend/1).
        explore(Query, Locality) :-
            explore(Query),
            expand(Locality),
            extend(Locality).

        % A solver given arguments built elsewhere (as a worker of the parallel solver, which has some of the rules) carries
        % on its exploration as they come: resume/1 explores again what they may support, restart/0 forgets it all.

        %! resume(+Statements)
        %  The exploration carried on once arguments for Statements are saved: the statements explored whose rules use
        %  them as premises, directly or through other statements, explored again, the others left as they are.
        resume(Statements) :-
            builtOn(Statements, [], Affected),
            findall(_, (member(S, Affected), context_retract(explored(S))), _),
            findall(_, (member(S, Affected), explore(S)), _).

        %! resumed(+Statements, +Queries)
        %  The exploration of Queries carried on once arguments for Statements, built elsewhere, are saved: from scratch
        %  when the conflicts are argued (see conflict::argued/0), as those for conflicts may make others possible
        %  attackers (see restart/0), or else exploring again what they may support (see resume/1).
        resumed(_, Queries) :-
            conflict::argued, !,
            restart,
            exploreAll(Queries).
        resumed(Statements, _) :-
            resume(Statements).

        %! restart
        %  The exploration forgotten, not the arguments it built: exploring again finds all that the arguments saved
        %  since support or make possible attackers (with metaConflicts, those for conflicts).
        restart :-
            context_retract(explored(_)),
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
            parser::rule(_, Premises, [Statement]),
            member(Premise, Premises),
            ground(Premise),
            parser::argued(Premise).

        %! exploreAll(+Queries)
        %  Each of Queries explored (see explore/1).
        exploreAll(Queries) :-
            findall(_, (member(Query, Queries), explore(Query)), _).

        %! buildSubGraph(+Query, ?Argument)
        %  Argument is each argument for Query, a premise, once explored.
        buildSubGraph(Query, Argument) :-
            exploreStatement(Query, premise),
            graphStore::argumentFor([Query], Argument).

        % exploreStatement(+Query, +Use): builds the arguments for Query, and those their labels may depend on (see
        % dependency/3), recursively, unless it is explored (once, or once more for each round of explore/1):
        % explored(Query) covers the statements Query generalises. A statement met again while it is being explored only
        % has the arguments built so far: needed as a premise (Use), that is a cycle; needed as a dependency, its
        % exploration under way builds them all.
        exploreStatement(Query, _) :-
            groundVars(Query, Normalized),
            context_absent(explored(Normalized)), !,
            context_assert(explored(Query)),
            context_assert(exploring(Normalized, Query)),
            findall(_, (
                argumentSearch::findSingleArgument([Query], Argument, Supports),
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

        % The statements whose arguments the label of an argument may depend on, besides its sub-arguments (built with
        % it): those of its possible attackers. The steps changing the graph relate it to others in turn (see extend/1).
        dependency(Id, Argument, S) :- attackCandidates::possibleAttackers(Id, Argument, [S]).

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

        % The statements whose arguments may depend on an argument: those it may attack, and those of its
        % super-arguments. The steps changing the graph relate others to it in turn (see extend/1).
        dependant(Argument, S) :- attackCandidates::possiblyAttacked(Argument, S).
        dependant(Argument, S) :-
            argument::claim(Argument, Claim),
            parser::ruleUsing(Claim, S).

        %! extend(+Locality)
        %  The exploration extended by the steps changing the graph (see structuredWhole::querySteps/1), as they extend
        %  the graph in whole mode: the statements each relates to the arguments built (queryDependencies/2, and for the
        %  connected locality queryDependants/2, which a step relating statements both defines) explored, until no
        %  argument is new. When the conflicts are arguments
        %  (see conflict::argued/0), the arguments built may make more of the others possible attackers or dependants:
        %  those are looked for again.
        extend(Locality) :-
            structuredWhole::querySteps(Steps),
            extend(Steps, Locality).

        extend([], _) :- !.
        extend(Steps, Locality) :-
            graphStore::argumentIds(Before),
            findall(S, (graphStore::argumentById(_, A), member(Step, Steps), related(Locality, Step, A, S)), Statements),
            findall(_, (member(S, Statements), explore(S)), _),
            graphStore::argumentIds(After),
            length(Before, N),
            length(After, M),
            (N == M -> true ; again(Locality), extend(Steps, Locality)).

        related(_, Step, Argument, S) :- Step::queryDependencies(Argument, S).
        related(connected, Step, Argument, S) :- Step::queryDependants(Argument, S).

        % Once arguments are added: if the conflicts are arguments, the possible attackers and the dependants of all
        % of them looked for again; those of the new ones otherwise
        again(Locality) :-
            conflict::argued, !,
            attackCandidates::forgetPossibleAttackers,
            context_retract(expanded(_)),
            findall(_, (graphStore::argumentById(Id, A), dependency(Id, A, S), explore(S)), _),
            expand(Locality).
        again(Locality) :-
            expand(Locality).

        % recordArgument(+Query, +Argument, +Supports): Argument saved with its supports, unless it already was (built in
        % a previous round of explore/1, whose dependencies are explored again)
        recordArgument(Query, Argument, Supports) :-
            graphStore::saveArgument(query, [Query], Argument), !,
            graphStore::saveSupporters(Argument, Supports).
        recordArgument(_, _, _).
        """.trimIndent()
}
