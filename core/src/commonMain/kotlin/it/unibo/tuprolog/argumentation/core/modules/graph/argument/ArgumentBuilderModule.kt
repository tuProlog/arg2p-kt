package it.unibo.tuprolog.argumentation.core.modules.graph.argument

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The arguments of a theory (module `argumentBuilder`): every argument its premises and rules make, built round after
 * round from the conclusions of the previous one, and saved in the context with their supports (see the module
 * graphStore).
 */
object ArgumentBuilderModule : PrologModule("prolog.argumentation.graph.arguments", "argumentBuilder") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! buildArguments
        %  The arguments made of a premise, those of the rules with no premise to support, and then, round after round,
        %  those of the rules using a conclusion of the previous round.
        buildArguments :-
            buildArgumentsFromPremises,
            buildArgumentsFromEmptyRules,
            findall([RuleID, RuleBody, RuleHead], (
                parser::classicRule(RuleID, RuleBody, RuleHead),
                \+ emptyRule([RuleID, RuleBody, RuleHead])
            ), Rules),
            context_retract(graphRule(_)),
            storeRules(Rules),
            findall(X, graphStore::newConclusion(a, [X]), N),
            buildArgumentsFromRules(N, b).

        % The rules are kept in the context rather than passed around: they are not ground, so carrying their list
        % in the goals makes every resolution step copy it. Asserted from the last, as context_check returns the
        % newest clauses first.
        storeRules([]).
        storeRules([Rule|Rules]) :-
            storeRules(Rules),
            context_assert(graphRule(Rule)).

        buildArgumentsFromPremises :-
            findall(_, (
                parser::classicRule(PremiseID, Premise),
                constraint::concludable(Premise),
                defeasibleIds(PremiseID, DefeasiblePremises),
                argument::infoParts(Info, [], [], DefeasiblePremises),
                argument::new([PremiseID], none, Premise, [], Info, Argument),
                graphStore::saveArgument(a, Premise, Argument)
            ), _).

        buildArgumentsFromEmptyRules :-
            findall(_, (
                emptyRule([RuleID, RulePrem, RuleHead]),
                ruleBodyIsSupported(RulePrem, rule(RuleID, RuleHead, RulePrem), [[], [], [], []], [], _),
                constraint::concludable(RuleHead),
                defeasibleIds(RuleID, DefeasibleRules),
                argument::infoParts(Info, DefeasibleRules, DefeasibleRules, []),
                constraint::extend(RulePrem, [], Body),
                argument::new([RuleID], RuleID, RuleHead, Body, Info, Argument),
                graphStore::saveArgument(a, RuleHead, Argument)
            ), _).

        % A rule with no premise to support with an argument: only contraries, Prolog checks and constraints
        emptyRule([RuleID, Body, RuleHead]) :-
            parser::classicRule(RuleID, Body, RuleHead),
            \+ (member(X, Body), X \= ~(_), X \= prolog(_), X \= '{}'(_)).

        % The defeasible ones among the rule or premise Id: none if strict
        defeasibleIds(Id, []) :- parser::checkStrict(Id), !.
        defeasibleIds(Id, [Id]).

        % A round fires every rule whose body uses a conclusion derived in the previous round (N), tagging the new
        % conclusions with Tn: the rounds go on while they derive something
        buildArgumentsFromRules(N, Tn) :-
            findall(_, (
                context_check(graphRule([Id, Body, Conc])),
                utils::containsAny(Body, N),
                buildArgumentsFromRule(Tn, [Id, Body, Conc])
            ), _),
            (graphStore::newConclusion(Tn, _) -> nextRound(Tn) ; true).

        buildArgumentsFromRule(Tn, [RuleID, RuleBody, RuleHead]) :-
            ruleBodyIsSupported(RuleBody, rule(RuleID, RuleHead, RuleBody), [[], [], [], []], [], Argument),
            graphStore::saveArgument(Tn, RuleHead, Argument).

        nextRound(Tn) :-
            otherTag(Tn, Tx),
            graphStore::forgetNewConclusions(Tx),
            findall(X, graphStore::newConclusion(Tn, [X]), N),
            buildArgumentsFromRules(N, Tx).

        otherTag(a, b).
        otherTag(b, a).

        ruleStrictness(Id, strict) :- parser::checkStrict(Id), !.
        ruleStrictness(_, defeasible).

        % ruleBodyIsSupported(Body, rule(RuleID, RuleHead, RuleBody), [All, LastDefRules, DefRules, DefPremises], Supports, Argument):
        % Argument is built from RuleID by supporting each statement in Body with a saved argument, collecting the rules
        % and premises of the supports, and the supports themselves: the constraints of the rule ({ } premises) and of the
        % supports, with their bindings, must hold together, and those still open are kept in the body of Argument (see
        % the module constraint); its conclusion may not be open but with the graph extension constraints, which saves a
        % support used for fewer values than it has as its instance. Only Body is matched in the clause heads, everything
        % else is unpacked in the bodies: matching heads with many arguments against every clause is what makes this
        % predicate expensive.
        ruleBodyIsSupported([], Rule, Acc, Supports, Argument) :-
            Rule = rule(RuleID, RuleHead, RuleBody),
            constraint::concludable(RuleHead),
            ruleStrictness(RuleID, Strict),
            argument::rulesAndInfo(RuleID, Strict, Acc, Rules, Info),
            constraint::extend(RuleBody, Supports, Body),
            argument::new(Rules, RuleID, RuleHead, Body, Info, Argument),
            \+ graphStore::argumentFor(RuleHead, Argument).
        ruleBodyIsSupported([~(_)|Others], Rule, Acc, Supports, Argument) :-
            ruleBodyIsSupported(Others, Rule, Acc, Supports, Argument).
        ruleBodyIsSupported([prolog(Check)|Others], Rule, Acc, Supports, Argument) :-
            (callable(Check) -> user::call(Check); user::Check),
            ruleBodyIsSupported(Others, Rule, Acc, Supports, Argument).
        ruleBodyIsSupported(['{}'(_)|Others], Rule, Acc, Supports, Argument) :-
            ruleBodyIsSupported(Others, Rule, Acc, Supports, Argument).
        ruleBodyIsSupported([Statement|Others], Rule, Acc, Supports, Argument) :-
            Acc = [All, LastDef, Def, Prem],
            graphStore::argumentFor([Statement], Support),
            argument::rules(Support, Rules),
            argument::info(Support, Info),
            argument::infoParts(Info, SupportLastDef, SupportDef, SupportPrem),
            ruleBodyIsSupported(Others, Rule, [[Rules|All], [SupportLastDef|LastDef], [SupportDef|Def], [SupportPrem|Prem]], [Support|Supports], Argument),
            (parser::instances -> constraints::realize(Support) ; true),
            graphStore::saveSupport(Support, Argument).
        """.trimIndent()
}
