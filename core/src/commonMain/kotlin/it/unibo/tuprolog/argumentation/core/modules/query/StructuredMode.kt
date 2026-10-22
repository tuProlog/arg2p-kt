package it.unibo.tuprolog.argumentation.core.modules.query

import it.unibo.tuprolog.argumentation.core.flags.AmbiguityBlocking
import it.unibo.tuprolog.argumentation.core.flags.QueryMode
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.PrologModule

/**
 * Query mode (module `structured`): the labels of the statements for a goal, computed on the part of the argumentation
 * graph they depend on (see the modules queryExploration and argumentSearch), or on the whole graph.
 */
object StructuredMode : PrologModule("prolog.argumentation.structured", "structured") {
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
        """.trimIndent()
}
