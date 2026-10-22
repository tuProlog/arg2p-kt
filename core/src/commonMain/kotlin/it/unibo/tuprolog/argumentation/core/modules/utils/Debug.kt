package it.unibo.tuprolog.argumentation.core.modules.utils

import it.unibo.tuprolog.argumentation.core.flags.DebugOutput
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.PrologModule

/**
 * The output of the labellings (module `debug`): printed by buildLabelSets/0, and written when the debug switch is on.
 */
object Debug : PrologModule("prolog.argumentation.debug", "debug") {
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(DebugOutput)

    override val prologRawTheory: String =
        """
        %! printArgumentationGraph(+Arguments, +Attacks, +Supports)
        %  The graph printed, its arguments part by part.
        printArgumentationGraph(Arguments, Attacks, Supports) :-
            findall(
                [Rules, '\n',  ' TOPRULE ',  TopRule, '\n', ' CONCLUSION ', Conclusion, '\n', ' BODY ', Body, '\n', ' INFO ', Info, '\n'],
                (member(Argument, Arguments), argument::new(Rules, TopRule, Conclusion, Body, Info, Argument)),
                ArgumentsToPrint
            ),
            findall((A1, ' SUPPORTS ', A2), member((A1, A2), Supports), SupportsToPrint),
            findall((A1, ' ', T, ' ', A2, ' ON ', A3),  member((T, A1, A2, A3), Attacks),  AttacksToPrint),
            write('HERE THE GROUNDED SEMI-ABSTRACT ARGUMENTATION GRAPH'),nl,
            utils::writeListNl(ArgumentsToPrint),
            utils::writeListNl(SupportsToPrint),
            utils::writeListNl(AttacksToPrint).


        %! printArgumentLabelling(+Labels)
        %  The arguments of [In, Out, Und] printed by label.
        printArgumentLabelling([IN, OUT, UND]) :-
            write('HERE THE ARGUMENTS LABELLED IN: '),nl,
            utils::writeListNl(IN),
            write('HERE THE ARGUMENTS LABELLED OUT: '),nl,
            utils::writeListNl(OUT),
            write('HERE THE ARGUMENTS LABELLED UND: '),nl,
            utils::writeListNl(UND).


        %! printStatementLabelling(+Labels)
        %  The statements of [In, Out, Und] printed by label.
        printStatementLabelling([In, Ni, Und]) :-
            write('HERE THE STATEMENTS LABELLED IN: '),nl,
            utils::writeListNl(In),
            write('HERE THE STATEMENTS LABELLED NI: '),nl,
            utils::writeListNl(Ni),
            write('HERE THE STATEMENTS LABELLED UND: '),nl,
            utils::writeListNl(Und).

        %! writeDebug(+List)
        %  The debug output is written with the debug switch on (setting(debug))
        writeDebug(List) :-
            setting(debug), !,
            utils::writeList(List).
        writeDebug(_).
        """.trimIndent()
}
