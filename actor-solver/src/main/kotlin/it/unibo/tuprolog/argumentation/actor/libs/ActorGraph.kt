package it.unibo.tuprolog.argumentation.actor.libs

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The graph explored one argument at a time (module `actorGraph`), by the solvers of the actors, which label it
 * themselves: the rules are distributed among them, each looking for the arguments and the attackers it can build with
 * its own. The steps changing the graph do not apply: their query relations are explored all the same (see
 * queryExploration::extend/1), so that the conflicts of metaConflicts make their attackers found.
 */
internal object ActorGraph : PrologModule("prolog.argumentation.actor.graph", "actorGraph") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! buildArgument(+Query, -Argument, -SubArguments)
        %  Argument is each argument for Query, saved with those its label may depend on, and SubArguments are its
        %  sub-arguments: the solvers that do not know its rules look for their attackers as well.
        buildArgument(Query, Argument, SubArguments) :-
            queryExploration::explore(Query),
            queryExploration::extend(directional),
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
            queryExploration::extend(directional),
            graphStore::argumentFor([Statement], Attacker),
            attackRelation::attacks(Type, Attacker, On),
            (parser::constrained -> constraint::compatible(Attacker, On) ; true).

        % The sub-arguments of an argument built by this solver
        subArguments(Argument, SubArguments) :-
            findall(S, subArgument(Argument, S), Ss),
            utils::deduplicate(Ss, SubArguments).

        subArgument(Argument, Sub) :-
            graphStore::support(Supporter, Argument),
            (Sub = Supporter ; subArgument(Supporter, Sub)).
        """.trimIndent()
}
