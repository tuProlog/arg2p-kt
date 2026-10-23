package it.unibo.tuprolog.argumentation.core.modules.query

import it.unibo.tuprolog.argumentation.core.flags.AmbiguityBlocking
import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.argumentation.core.flags.QueryMode
import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * Query mode (module `structured`): the labels of the statements for a goal, computed on the part of the argumentation
 * graph they depend on (see the modules queryExploration and argumentSearch), or on the whole graph.
 */
object StructuredModule : PrologModule("prolog.argumentation.structured", "structured") {
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(QueryMode, AmbiguityBlocking)

    override val prologRawTheory: String =
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
            abstract:::computeGlobalAcceptance.
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
            abstract:::computeGlobalAcceptance.
        queryAcceptance(Locality, Goal) :-
            queryExploration::explore(Goal),
            queryExploration::expand(Locality),
            setting(graphBuildMode, X),
            X:::buildAdditionalArguments,
            X:::buildAttacks,
            abstract:::modifyGraph,
            abstract:::buildArgumentLabelling,
            abstract:::buildStatementLabelling.

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
        % distributes the arguments and their attackers among its workers). The graph extensions do not apply.

        %! buildArgument(+Query, -Argument)
        %  Argument is each argument for Query, saved with those its label may depend on.
        buildArgument(Query, Argument) :-
            queryExploration::explore(Query),
            graphStore::argumentFor([Query], Argument).

        %! findAttacker(+Target, +QueryChain, -Attacker, -IsValid)
        %  Attacker attacks Target, directly or on one of its sub-arguments, and the attack is saved; IsValid is no when
        %  Attacker is in QueryChain (the arguments the evaluation of Target comes from: a cycle), yes otherwise. Target
        %  may have been built by another solver, with rules this one does not know.
        findAttacker(Target, QueryChain, Attacker, IsValid) :-
            attacker(Target, Attacker, Type, On),
            graphStore::saveAttack(Type, Attacker, Target, On),
            (member(Attacker, QueryChain) -> IsValid = no ; IsValid = yes).

        % The undercuts of any rule or premise of Target not known to be strict (as the actor solver always had them:
        % the attacks of the whole graph only undercut the top rules of the arguments)
        attacker(Target, Attacker, undercut, Target) :-
            argument::rules(Target, Rules),
            utils::deduplicate(Rules, Ids),
            member(Rule, Ids),
            \+ parser::check_strict(Rule),
            queryExploration::explore(undercut(Rule)),
            graphStore::argumentFor([undercut(Rule)], Attacker).
        % The other attacks on Target, and on its sub-arguments known to this solver
        attacker(Target, Attacker, Type, On) :-
            subArgument(Target, On),
            attackerOf(On, Attacker, Type),
            Type \== undercut.

        % On is Argument or one of its sub-arguments
        subArgument(Argument, Argument).
        subArgument(Argument, On) :-
            graphStore::support(Supporter, Argument),
            subArgument(Supporter, On).

        % Attacker attacks On directly, with an attack of Type: among the arguments for the statements that may attack
        % it, built as needed
        attackerOf(On, Attacker, Type) :-
            findall(S, attackCandidates::findPossibleAttackers(On, [S]), Statements),
            utils::deduplicate(Statements, Candidates),
            member(Statement, Candidates),
            queryExploration::explore(Statement),
            graphStore::argumentFor([Statement], Attacker),
            attackRelation::attacks(Type, Attacker, On).
        """.trimIndent()
}
