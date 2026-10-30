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
            mode(Mode),
            buildArgumentsFromPremises(Mode),
            buildArgumentsFromEmptyRules(Mode),
            findall([RuleID, RuleBody, RuleHead], (
                parser::classicRule(RuleID, RuleBody, RuleHead),
                \+ emptyRule([RuleID, RuleBody, RuleHead])
            ), Rules),
            context_retract(graphRule(_)),
            storeRules(Rules),
            findall(X, graphStore::newConclusion(a, [X]), N),
            buildArgumentsFromRules(N, b, Mode).

        % mode(-Mode): base, for a theory whose arguments all conclude ground statements with no constraint to keep, or
        % constrained(Instances) (parser::constrained/0), Instances yes with the instances of the graph extension
        % constraints: asked once, the arguments of a theory in the base mode built as if constraints were not there
        mode(constrained(Instances)) :- parser::constrained, !, (parser::instances -> Instances = yes ; Instances = no).
        mode(base).

        % The rules are kept in the context rather than passed around: they are not ground, so carrying their list
        % in the goals makes every resolution step copy it. Asserted from the last, as context_check returns the
        % newest clauses first.
        storeRules([]).
        storeRules([Rule|Rules]) :-
            storeRules(Rules),
            context_assert(graphRule(Rule)).

        buildArgumentsFromPremises(Mode) :-
            findall(_, (
                parser::classicRule(PremiseID, Premise),
                concludable(Mode, Premise),
                defeasibleIds(PremiseID, DefeasiblePremises),
                argument::infoParts(Info, [], [], DefeasiblePremises),
                argument::new([PremiseID], none, Premise, [], Info, Argument),
                graphStore::saveArgument(a, Premise, Argument)
            ), _).

        buildArgumentsFromEmptyRules(Mode) :-
            findall(_, (
                emptyRule([RuleID, RulePrem, RuleHead]),
                supported(Mode, RulePrem, RuleID, RuleHead, _),
                defeasibleIds(RuleID, DefeasibleRules),
                argument::infoParts(Info, DefeasibleRules, DefeasibleRules, []),
                withConstraints(Mode, RulePrem, [], Body),
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

        %! buildArgumentsFromRules(+N, +Tn)
        %  The rounds of the rules using the conclusions N (see buildArgumentsFromRules/3), for the solvers building the
        %  arguments of a part of the rules (the workers of the parallel solver).
        buildArgumentsFromRules(N, Tn) :-
            mode(Mode),
            buildArgumentsFromRules(N, Tn, Mode).

        % A round fires every rule whose body uses a conclusion derived in the previous round (N), tagging the new
        % conclusions with Tn: the rounds go on while they derive something
        buildArgumentsFromRules(N, Tn, Mode) :-
            findall(_, (
                context_check(graphRule([Id, Body, Conc])),
                utils::containsAny(Body, N),
                buildArgumentsFromRule(Tn, [Id, Body, Conc], Mode)
            ), _),
            (graphStore::newConclusion(Tn, _) -> nextRound(Tn, Mode) ; true).

        buildArgumentsFromRule(Tn, [RuleID, RuleBody, RuleHead], Mode) :-
            supported(Mode, RuleBody, RuleID, RuleHead, Argument),
            graphStore::saveArgument(Tn, RuleHead, Argument).

        nextRound(Tn, Mode) :-
            otherTag(Tn, Tx),
            graphStore::forgetNewConclusions(Tx),
            findall(X, graphStore::newConclusion(Tn, [X]), N),
            buildArgumentsFromRules(N, Tx, Mode).

        otherTag(a, b).
        otherTag(b, a).

        ruleStrictness(Id, strict) :- parser::checkStrict(Id), !.
        ruleStrictness(_, defeasible).

        % supported(+Mode, +Body, +RuleID, +RuleHead, -Argument): Argument is built from the rule RuleID supporting each
        % statement of its premises Body with a saved argument, as the mode of the theory asks
        supported(base, Body, RuleID, RuleHead, Argument) :-
            ruleBodyIsSupported(Body, rule(RuleID, RuleHead, Body), [[], [], [], []], Argument).
        supported(constrained(Instances), Body, RuleID, RuleHead, Argument) :-
            constrainedBodyIsSupported(Body, rule(RuleID, RuleHead, Body, Instances), [[], [], [], []], [], Argument).

        % An argument may conclude Conclusion: ground, or as the module constraint says in a theory with constraints
        concludable(_, Conclusion) :- ground(Conclusion), !.
        concludable(constrained(_), Conclusion) :- constraint::concludable(Conclusion).

        % The body of the argument: the premises of its rule, with the constraints of its supports still open in a theory
        % with constraints (see constraint::extend/3)
        withConstraints(base, Premises, _, Premises).
        withConstraints(constrained(_), Premises, Supports, Body) :- constraint::extend(Premises, Supports, Body).

        % ruleBodyIsSupported(Body, rule(RuleID, RuleHead, RuleBody), [All, LastDefRules, DefRules, DefPremises], Argument):
        % Argument is built from RuleID by supporting each statement in Body with a saved argument, collecting the rules
        % and premises of the supports; its conclusion is ground. Only Body is matched in the clause heads, everything
        % else is unpacked in the bodies: matching heads with many arguments against every clause is what makes this
        % predicate expensive.
        ruleBodyIsSupported([], Rule, Acc, Argument) :-
            Rule = rule(RuleID, RuleHead, RuleBody),
            ground(RuleHead),
            ruleStrictness(RuleID, Strict),
            argument::rulesAndInfo(RuleID, Strict, Acc, Rules, Info),
            argument::new(Rules, RuleID, RuleHead, RuleBody, Info, Argument),
            \+ graphStore::argumentFor(RuleHead, Argument).
        ruleBodyIsSupported([~(_)|Others], Rule, Acc, Argument) :-
            ruleBodyIsSupported(Others, Rule, Acc, Argument).
        ruleBodyIsSupported([prolog(Check)|Others], Rule, Acc, Argument) :-
            (callable(Check) -> user::call(Check); user::Check),
            ruleBodyIsSupported(Others, Rule, Acc, Argument).
        ruleBodyIsSupported([Statement|Others], Rule, Acc, Argument) :-
            Acc = [All, LastDef, Def, Prem],
            graphStore::argumentFor([Statement], Support),
            argument::rules(Support, Rules),
            argument::info(Support, Info),
            argument::infoParts(Info, SupportLastDef, SupportDef, SupportPrem),
            ruleBodyIsSupported(Others, Rule, [[Rules|All], [SupportLastDef|LastDef], [SupportDef|Def], [SupportPrem|Prem]], Argument),
            graphStore::saveSupport(Support, Argument).

        % constrainedBodyIsSupported(Body, rule(RuleID, RuleHead, RuleBody, Instances), Acc, Supports, Argument): as
        % ruleBodyIsSupported/4 in a theory with constraints, collecting the supports as well: the constraints of the rule
        % ({ } premises) and of the supports, with their bindings, must hold together, and those still open are kept in
        % the body of Argument (see the module constraint); with instances (Instances yes) its conclusion may be open, and
        % a support used for fewer values than it has is saved as its instance (see the module constraints).
        constrainedBodyIsSupported([], Rule, Acc, Supports, Argument) :-
            Rule = rule(RuleID, RuleHead, RuleBody, Instances),
            concludable(constrained(Instances), RuleHead),
            ruleStrictness(RuleID, Strict),
            argument::rulesAndInfo(RuleID, Strict, Acc, Rules, Info),
            constraint::extend(RuleBody, Supports, Body),
            argument::new(Rules, RuleID, RuleHead, Body, Info, Argument),
            \+ graphStore::argumentFor(RuleHead, Argument).
        constrainedBodyIsSupported([~(_)|Others], Rule, Acc, Supports, Argument) :-
            constrainedBodyIsSupported(Others, Rule, Acc, Supports, Argument).
        constrainedBodyIsSupported([prolog(Check)|Others], Rule, Acc, Supports, Argument) :-
            (callable(Check) -> user::call(Check); user::Check),
            constrainedBodyIsSupported(Others, Rule, Acc, Supports, Argument).
        constrainedBodyIsSupported(['{}'(_)|Others], Rule, Acc, Supports, Argument) :-
            constrainedBodyIsSupported(Others, Rule, Acc, Supports, Argument).
        constrainedBodyIsSupported([Statement|Others], Rule, Acc, Supports, Argument) :-
            Acc = [All, LastDef, Def, Prem],
            graphStore::argumentFor([Statement], Support),
            argument::rules(Support, Rules),
            argument::info(Support, Info),
            argument::infoParts(Info, SupportLastDef, SupportDef, SupportPrem),
            constrainedBodyIsSupported(Others, Rule, [[Rules|All], [SupportLastDef|LastDef], [SupportDef|Def], [SupportPrem|Prem]], [Support|Supports], Argument),
            Rule = rule(_, _, _, Instances),
            (Instances == yes -> constraints::realize(Support) ; true),
            graphStore::saveSupport(Support, Argument).
        """.trimIndent()
}
