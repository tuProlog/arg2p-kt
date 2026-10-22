package it.unibo.tuprolog.argumentation.core.libs.utils

import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.PrologModule

object Debug : PrologModule("prolog.argumentation.debug", "debug") {
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(DebugOutput)

    override val prologRawTheory: String =
        """
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


        printArgumentLabelling([IN, OUT, UND]) :-
            write('HERE THE ARGUMENTS LABELLED IN: '),nl,
            utils::writeListNl(IN),
            write('HERE THE ARGUMENTS LABELLED OUT: '),nl,
            utils::writeListNl(OUT),
            write('HERE THE ARGUMENTS LABELLED UND: '),nl,
            utils::writeListNl(UND).


        printStatementLabelling([In, Ni, Und]) :-
            write('HERE THE STATEMENTS LABELLED IN: '),nl,
            utils::writeListNl(In),
            write('HERE THE STATEMENTS LABELLED NI: '),nl,
            utils::writeListNl(Ni),
            write('HERE THE STATEMENTS LABELLED UND: '),nl,
            utils::writeListNl(Und).

        % The debug output is written with the debug switch on (setting(debug))
        writeDebug(List) :-
            setting(debug), !,
            utils::writeList(List).
        writeDebug(_).
        """.trimIndent()
}

// The debug output of the modules (writeDebug/1), off unless the theory states it (debug.)
object DebugOutput : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "debug"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = false

    override fun values() {}
}
