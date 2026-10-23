package it.unibo.tuprolog.argumentation.core.modules.graph

import it.unibo.tuprolog.argumentation.core.flags.AmbiguityBlocking
import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.argumentation.core.flags.QueryMode
import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * Query mode (module `structuredPartial`): the labels of the statements for a goal, computed on the part of the argumentation
 * graph they depend on (see the modules queryExploration and argumentSearch), or on the whole graph.
 */
object StructuredPartialModule : PrologModule("prolog.argumentation.structured.partial", "structuredPartial") {
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(QueryMode, AmbiguityBlocking)

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! computeStatementAcceptance(+Goal, -Result)
        %  The labels of the statements for Goal: in(S), out(S) and und(S) terms.
        computeStatementAcceptance(Goal, Res) :-
            computeStatementAcceptance(Goal),
            mineResults(Goal, Res).

        %! computeStatementAcceptance(+Goal, -In, -Out, -Und)
        %  The statements for Goal labelled IN, OUT and UND.
        computeStatementAcceptance(Goal, YesResult, NoResult, UndResult) :-
            computeStatementAcceptance(Goal),
            mineResults(Goal, YesResult, NoResult, UndResult).

        %! computeStatementAcceptance(+Goal)
        %  The labels of the statements for Goal, in the context: with queryMode only the part of the graph they depend
        %  on is built, otherwise the whole graph.
        computeStatementAcceptance(Goal) :-
            \+ setting(queryMode),
            structuredWhole:::computeGlobalAcceptance.
        computeStatementAcceptance(Goal) :-
            setting(queryMode),
            locality(Locality),
            queryAcceptance(Locality, Goal).

        %! locality(-Locality)
        %  The part of the graph the labels of the arguments for the query depend on, as the semantics declares it
        %  (locality/1): the arguments they depend on (directional), those connected to them in either direction
        %  (connected), or the whole graph (whole_graph, also for a semantics that does not declare it)
        locality(Locality) :-
            setting(argumentLabellingMode, Semantics),
            Semantics::locality(L), !,
            Locality = L.
        locality(whole_graph).

        queryAcceptance(whole_graph, _) :- !,
            structuredWhole:::computeGlobalAcceptance.
        queryAcceptance(Locality, Goal) :-
            queryExploration::explore(Goal),
            queryExploration::expand(Locality),
            setting(graphBuildMode, X),
            X:::buildAdditionalArguments,
            X:::buildAttacks,
            structuredWhole:::modifyGraph,
            structuredWhole:::buildArgumentLabelling,
            structuredWhole:::buildStatementLabelling.

        mineResults(Goal, YesResult, NoResult, UndResult) :-
            findall(Goal, graphStore::statementLabel(in, [Goal]), In),
            findall(Goal, graphStore::statementLabel(out, [Goal]), Out),
            findall(Goal, graphStore::statementLabel(und, [Goal]), Und),
            utils::sort(In, YesResult),
            utils::sort(Out, NoResult),
            utils::sort(Und, UndResult).

        mineResults(Goal, Result) :-
            findall(in(Goal), graphStore::statementLabel(in, [Goal]), In),
            findall(out(Goal), graphStore::statementLabel(out, [Goal]), Out),
            findall(und(Goal), graphStore::statementLabel(und, [Goal]), Und),
            utils::appendLists([In, Out, Und], Result).

        % The graph explored one argument at a time, for the solvers that label it themselves (the actor solver
        % distributes the rules among its workers, each solver looking for the arguments and the attackers it can build
        % with its own). The graph extensions do not apply.

        %! buildArgument(+Query, -Argument, -SubArguments)
        %  Argument is each argument for Query, saved with those its label may depend on, and SubArguments are its
        %  sub-arguments: the solvers that do not know its rules look for their attackers as well.
        buildArgument(Query, Argument, SubArguments) :-
            queryExploration::explore(Query),
            graphStore::argumentFor([Query], Argument),
            subArguments(Argument, SubArguments).

        %! findAttacker(+Target, +SubArguments, +QueryChain, -Attacker, -AttackerSubArguments, -IsValid)
        %  Attacker attacks Target, directly or on one of its SubArguments, and the attacks are saved; each attacker is
        %  given once, with its own sub-arguments. IsValid is no when Attacker is in QueryChain (the arguments the
        %  evaluation of Target comes from: a cycle), yes otherwise. Target may have been built by another solver, with
        %  rules this one does not know: its sub-arguments come with it.
        findAttacker(Target, SubArguments, QueryChain, Attacker, AttackerSubArguments, IsValid) :-
            findall(A, attacker(Target, [Target|SubArguments], A), As),
            utils::deduplicate(As, Attackers),
            member(Attacker, Attackers),
            subArguments(Attacker, AttackerSubArguments),
            (member(Attacker, QueryChain) -> IsValid = no ; IsValid = yes).

        % Attacker attacks Target on one of Arguments, Target and its sub-arguments, and the attack is saved (unless it
        % already is: the same argument is evaluated again in another query chain)
        attacker(Target, Arguments, Attacker) :-
            member(On, Arguments),
            attackerOf(On, Attacker, Type),
            (graphStore::saveAttack(Type, Attacker, Target, On) -> true ; true).

        % Attacker attacks On directly, with an attack of Type: among the arguments for the statements that may attack
        % it, built as needed
        attackerOf(On, Attacker, Type) :-
            findall(S, attackCandidates::findPossibleAttackers(On, [S]), Statements),
            utils::deduplicate(Statements, Candidates),
            member(Statement, Candidates),
            queryExploration::explore(Statement),
            graphStore::argumentFor([Statement], Attacker),
            attackRelation::attacks(Type, Attacker, On).

        % The sub-arguments of an argument built by this solver
        subArguments(Argument, SubArguments) :-
            findall(S, subArgument(Argument, S), Ss),
            utils::deduplicate(Ss, SubArguments).

        subArgument(Argument, Sub) :-
            graphStore::support(Supporter, Argument),
            (Sub = Supporter ; subArgument(Supporter, Sub)).
        """.trimIndent()
}
