package it.unibo.tuprolog.argumentation.core.libs.structured

import it.unibo.tuprolog.argumentation.core.libs.PrologModule
import it.unibo.tuprolog.argumentation.core.libs.basic.Arg2pUnificator
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * The part of the argumentation graph a query depends on (module `queryExploration`, for query mode): the arguments
 * for the query, and, recursively, those their labels may depend on, saved as they are built (see the module
 * argumentSearch).
 */
object QueryExploration : PrologModule("prolog.argumentation.structured.exploration", "queryExploration") {
    override val primitives: Map<Signature, Primitive>
        get() = mapOf(GroundVars.descriptionPair)

    override val prologRawTheory: String =
        """
        %! explore(+Query)
        %  The arguments for Query and those their labels may depend on (see exploreStatement/2). With cycles among the
        %  rules, a statement needed as a premise while its arguments are being built only gets those built so far: the
        %  exploration is then repeated, from scratch, until it builds no new argument (the arguments are finitely many,
        %  as none uses a rule twice).
        explore(Query) :-
            context_retract(newConc(query, _)),
            context_retract(cycle),
            exploreStatement(Query, dependency),
            (context_check(cycle), context_check(newConc(query, _)) ->
                context_retract(explored(_)),
                explore(Query)
            ;
                true
            ).

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
            ground_vars(Query, Normalized),
            context_absent(explored(Normalized)), !,
            context_assert(explored(Query)),
            context_assert(exploring(Normalized, Query)),
            findall(_, (
                argumentSearch::findSingleArgument([Query], Argument, Supports),
                recordArgument(Query, Argument, Supports),
                context_id(argument(Argument), Id),
                dependency(Id, Argument, Statement),
                exploreStatement(Statement, dependency)
            ), _),
            context_retract(exploring(Normalized, _)).
        exploreStatement(Query, premise) :-
            ground_vars(Query, Normalized),
            context_check(exploring(_, Normalized)), !,
            (context_absent(cycle) -> context_assert(cycle) ; true).
        exploreStatement(_, _).

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

// ground_vars(+Term, ?Grounded): Term with each of its variables replaced by the atom var
object GroundVars : BinaryRelation.WithoutSideEffects<ExecutionContext>("ground_vars") {
    private val placeholder = Atom.of("var")

    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> =
        Arg2pUnificator
            .mgu(second, first.apply(Substitution.of(first.variables.distinct().associateWith { placeholder })))
            .let { if (it.isSuccess) sequenceOf(it) else emptySequence() }
}
