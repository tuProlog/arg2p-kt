package it.unibo.tuprolog.argumentation.core.modules.graph

import it.unibo.tuprolog.argumentation.core.flags.AnswerMode
import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.argumentation.core.flags.QueryMode
import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * Query mode (module `structuredPartial`): the labels of the statements for a goal, computed on the part of the argumentation
 * graph they depend on (see the modules queryExploration and argumentSearch), or on the whole graph.
 */
object StructuredPartialModule : PrologModule("prolog.argumentation.structured.partial", "structuredPartial") {
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(QueryMode, AnswerMode)

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! computeStatementAcceptance(+Goal, -Result)
        %  The labels of the statements for Goal: in(S), out(S) and und(S) terms.
        computeStatementAcceptance(Goal, Res) :-
            setting(answerMode, enumerate), !,
            computeStatementAcceptance(Goal),
            mineResults(Goal, Res).
        computeStatementAcceptance(Goal, Res) :-
            computeStatementAcceptance(Goal, In, Out, Und),
            findall(R, (member(S, In), R = in(S) ; member(S, Out), R = out(S) ; member(S, Und), R = und(S)), Res).

        %! computeStatementAcceptance(+Goal, -In, -Out, -Und)
        %  The statements for Goal labelled IN, OUT and UND, as answerMode states: in each labelling, one solution for
        %  each (enumerate), or across them all (credulous, skeptical, see acceptance/5).
        computeStatementAcceptance(Goal, YesResult, NoResult, UndResult) :-
            setting(answerMode, enumerate), !,
            computeStatementAcceptance(Goal),
            mineResults(Goal, YesResult, NoResult, UndResult).
        computeStatementAcceptance(Goal, YesResult, NoResult, UndResult) :-
            setting(answerMode, Mode),
            findall([Yes, No, Und], (computeStatementAcceptance(Goal), mineResults(Goal, Yes, No, Und)), Labellings),
            acceptance(Mode, Labellings, YesResult, NoResult, UndResult).

        %! accepted(?Goal)
        %  Goal is a statement labelled IN, as answerMode states (see computeStatementAcceptance/4).
        accepted(Goal) :-
            setting(answerMode, enumerate), !,
            computeStatementAcceptance(Goal),
            answer(in, Goal).
        accepted(Goal) :-
            computeStatementAcceptance(Goal, In, _, _),
            member(Goal, In).

        % acceptance(+Mode, +Labellings, -In, -Out, -Und): the statements across Labellings, the [In, Out, Und] of each
        % labelling. Credulous: IN those IN in some labelling, OUT those OUT in every one. Skeptical: IN those IN in
        % every labelling, OUT those OUT in some. UND the others: none is in two of the three.
        acceptance(Mode, Labellings, In, Out, Und) :-
            findall(S, (member(Labelling, Labellings), member(Statements, Labelling), member(S, Statements)), Found),
            distinctStatements(Found, [], All),
            findall(S, (member(S, All), acceptedAs(Mode, in, S, Labellings)), In0),
            findall(S, (member(S, All), \+ acceptedAs(Mode, in, S, Labellings), acceptedAs(Mode, out, S, Labellings)), Out0),
            findall(S, (member(S, All), \+ acceptedAs(Mode, in, S, Labellings), \+ acceptedAs(Mode, out, S, Labellings)), Und0),
            utils::sortDescending(In0, In),
            utils::sortDescending(Out0, Out),
            utils::sortDescending(Und0, Und).

        acceptedAs(credulous, in, S, Labellings) :- member([Yes, _, _], Labellings), memberStatement(S, Yes), !.
        acceptedAs(credulous, out, S, Labellings) :- \+ (member([_, No, _], Labellings), \+ memberStatement(S, No)).
        acceptedAs(skeptical, in, S, Labellings) :- \+ (member([Yes, _, _], Labellings), \+ memberStatement(S, Yes)).
        acceptedAs(skeptical, out, S, Labellings) :- member([_, No, _], Labellings), memberStatement(S, No), !.

        distinctStatements([], Distinct, Distinct).
        distinctStatements([S|Ss], Seen, Distinct) :-
            (memberStatement(S, Seen) -> distinctStatements(Ss, Seen, Distinct) ; distinctStatements(Ss, [S|Seen], Distinct)).

        % S is one of the statements, the same up to the names of its variables (each labelling has its own copies)
        memberStatement(S, [T|_]) :- sameStatement(S, T), !.
        memberStatement(S, [_|Ts]) :- memberStatement(S, Ts).

        sameStatement(S, T) :- ground(S), !, S == T.
        sameStatement(S, T) :- constraint::subsumes(S, T), constraint::subsumes(T, S).

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
        %  (connected), or the whole graph (whole_graph, also for a semantics that does not declare it). Only rules are
        %  explored: a graph built otherwise (a graphBuilder other than ruleGraph, as for an abstract theory) is whole.
        locality(Locality) :-
            structuredWhole::builder(X),
            (X == ruleGraph -> semanticsLocality(Locality) ; Locality = whole_graph).

        semanticsLocality(Locality) :-
            setting(argumentLabellingMode, Semantics),
            (module_defines(Semantics, locality/1) -> Semantics::locality(Locality) ; Locality = whole_graph).

        % The part of the graph explored, its attacks, then changed and labelled as the whole graph is (see the module
        % structuredWhole)
        queryAcceptance(whole_graph, _) :- !,
            structuredWhole:::computeGlobalAcceptance.
        queryAcceptance(Locality, Goal) :-
            structuredWhole::checkProfile,
            queryExploration::explore(Goal, Locality),
            ruleGraph:::buildAttacks,
            structuredWhole:::modifyGraph,
            structuredWhole:::buildArgumentLabelling,
            structuredWhole:::buildStatementLabelling.

        mineResults(Goal, YesResult, NoResult, UndResult) :-
            findall(Goal, answer(in, Goal), In),
            findall(Goal, answer(out, Goal), Out),
            findall(Goal, answer(und, Goal), Und),
            utils::sortDescending(In, YesResult),
            utils::sortDescending(Out, NoResult),
            utils::sortDescending(Und, UndResult).

        mineResults(Goal, Result) :-
            findall(in(Goal), answer(in, Goal), In),
            findall(out(Goal), answer(out, Goal), Out),
            findall(und(Goal), answer(und, Goal), Und),
            utils::appendLists([In, Out, Und], Result).

        %! answer(?Label, ?Goal)
        %  A statement labelled Label answering Goal: one unifying with it, or one of its instances, as an
        %  argument holding for more values than Goal asks about says nothing of them alone (its instance for Goal does).
        answer(Label, Goal) :-
            parser::instances, !,
            graphStore::statementLabel(Label, [Statement]),
            constraint::subsumes(Goal, Statement),
            Goal = Statement.
        answer(Label, Goal) :-
            graphStore::statementLabel(Label, [Goal]).
        """.trimIndent()
}
