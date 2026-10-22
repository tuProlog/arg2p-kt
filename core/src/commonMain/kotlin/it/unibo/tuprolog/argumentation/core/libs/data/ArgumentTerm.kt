package it.unibo.tuprolog.argumentation.core.libs.data

import it.unibo.tuprolog.argumentation.core.libs.PrologModule
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.core.List as LogicList

/**
 * The term of an argument (module `argument`), read and built through it rather than by the positions of its parts.
 *
 * An argument is the list `[Rules, TopRule, Conclusion, Body, Info]`:
 * - Rules: the identifiers of the rules and premises it is made of;
 * - TopRule: the identifier of its last rule, `none` for an argument made of a premise alone;
 * - Conclusion: the statement it concludes, `[Claim]`;
 * - Body: the premises of its top rule;
 * - Info: `[LastDefeasibleRules, DefeasibleRules, DefeasiblePremises]`, what the preferences between arguments weigh.
 *
 * Each accessor is a fact relating an argument to one of its parts, so it works both ways: given an argument it
 * reads the part, given a part it builds the template of the arguments with it, to look them up in the context
 * (`argument::claim(A, sup(X, Y)), context_check(argument(A))`). Kotlin reads and builds the terms through [parts] and
 * [info], written after the same layout.
 */
object ArgumentTerm : PrologModule("prolog.argumentation.data.argument", "argument") {
    override val primitives: Map<Signature, Primitive>
        get() = mapOf(ArgumentRules.signature to ArgumentRules)

    override val prologRawTheory: String =
        """
        %! new(?Rules, ?TopRule, ?Conclusion, ?Body, ?Info, ?Argument)
        %  Argument is made of its parts.
        new(Rules, TopRule, Conclusion, Body, Info, [Rules, TopRule, Conclusion, Body, Info]).

        %! rules(?Argument, ?Rules)
        %  Rules are the identifiers of the rules and premises of Argument.
        rules([Rules, _, _, _, _], Rules).

        %! withRules(?Argument, ?Rules, ?Changed)
        %  Changed is Argument with the rules Rules.
        withRules([_, TopRule, Conclusion, Body, Info], Rules, [Rules, TopRule, Conclusion, Body, Info]).

        %! topRule(?Argument, ?TopRule)
        %  TopRule is the last rule of Argument, none for an argument made of a premise alone.
        topRule([_, TopRule, _, _, _], TopRule).

        %! conclusion(?Argument, ?Conclusion)
        %  Conclusion is the statement Argument concludes, [Claim].
        conclusion([_, _, Conclusion, _, _], Conclusion).

        %! claim(?Argument, ?Claim)
        %  Claim is the term Argument concludes.
        claim([_, _, [Claim], _, _], Claim).

        %! body(?Argument, ?Body)
        %  Body are the premises of the top rule of Argument.
        body([_, _, _, Body, _], Body).

        %! info(?Argument, ?Info)
        %  Info is what the preferences weigh of Argument: [LastDefeasibleRules, DefeasibleRules, DefeasiblePremises].
        info([_, _, _, _, Info], Info).

        %! lastDefeasibleRules(?Argument, ?Rules)
        %  Rules are the last defeasible rules of Argument.
        lastDefeasibleRules([_, _, _, _, [Rules, _, _]], Rules).

        %! defeasibleRules(?Argument, ?Rules)
        %  Rules are the defeasible rules of Argument.
        defeasibleRules([_, _, _, _, [_, Rules, _]], Rules).

        %! defeasiblePremises(?Argument, ?Premises)
        %  Premises are the defeasible premises of Argument.
        defeasiblePremises([_, _, _, _, [_, _, Premises]], Premises).

        %! infoParts(?Info, ?LastDefeasibleRules, ?DefeasibleRules, ?DefeasiblePremises)
        %  Info is made of its parts.
        infoParts([LastDefeasibleRules, DefeasibleRules, DefeasiblePremises], LastDefeasibleRules, DefeasibleRules, DefeasiblePremises).

        %! isPremise(+Argument)
        %  Argument is made of a premise alone.
        isPremise([_, TopRule, _, _, _]) :- TopRule == none.

        %! rulesAndInfo(+RuleId, +Strict, +Supports, -Rules, -Info)
        %  The rules and the info of the argument built from the rule RuleId (strict if Strict is strict) on sub-arguments
        %  with the rules, last defeasible rules, defeasible rules and defeasible premises in Supports (see ArgumentRules);
        %  native.

        %! isDefeasible(+Argument)
        %  Argument has defeasible rules or premises.
        isDefeasible([_, _, _, _, Info]) :- Info \== [[], [], []].
        """.trimIndent()

    /** The parts of the term of an argument, by name */
    class Parts(
        val rules: Term,
        val topRule: Term,
        val conclusion: Term,
        val body: Term,
        val lastDefeasibleRules: Term,
        val defeasibleRules: Term,
        val defeasiblePremises: Term,
    ) {
        /** The term the argument concludes, the single item of its conclusion */
        val claim: Term?
            get() = (conclusion as? LogicList)?.toList()?.singleOrNull()
    }

    /** The parts of [argument], the term of an argument; null if it is not one */
    fun parts(argument: Term): Parts? {
        val items = (argument as? LogicList)?.toList()?.takeIf { it.size == 5 } ?: return null
        val info = (items[4] as? LogicList)?.toList()?.takeIf { it.size == 3 } ?: return null
        return Parts(items[0], items[1], items[2], items[3], info[0], info[1], info[2])
    }

    /** The term of an argument, made of its parts */
    fun of(
        rules: Term,
        topRule: Term,
        conclusion: Term,
        body: Term,
        info: Term,
    ): Term = LogicList.of(rules, topRule, conclusion, body, info)

    /** The info of an argument, made of its parts */
    fun info(
        lastDefeasibleRules: Term,
        defeasibleRules: Term,
        defeasiblePremises: Term,
    ): Term = LogicList.of(lastDefeasibleRules, defeasibleRules, defeasiblePremises)
}
