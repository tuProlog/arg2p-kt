package it.unibo.tuprolog.argumentation.behaviour.causality

import it.unibo.tuprolog.argumentation.Arg2p
import kotlin.test.Test

// Causation over time: the theories state when the events happen and what holds at each time
class TemporalExamplesTest {
    // The successor of a time, and the times after it, up to t
    private val temporalDomain = { t: Int ->
        """
        next(N, M) :- integer(N), !, M is N + 1, N >= 0, M =< $t.
        next(N, M) :- integer(M), !, N is M - 1, N >= 0, M =< $t.

        nextD(N, M) :- next(N, M).
        nextD(N, M) :- integer(N), !, next(N, M1), nextD(M1, M).
        nextD(N, M) :- integer(M), !, next(N1, M), nextD(N, N1).
        """.trimIndent()
    }

    // Facts with variables (d1, c1) conclude open statements: their instances, as the rules use them, need the graph
    // extension constraints
    private val schemata = "\ngraphExtension(standardPref).\ngraphExtension(rebutRestriction).\ngraphExtension(constraints).\n"

    @Test
    fun doubleShootingTemporal(): Unit =
        Arg2p
            .of(
                """
                r_1(X, Y, T) : sh(X, Y, T), prolog(next(T, T1)) => hit(X, Y, T1).
                r_2(X, Y, T) : hit(X, Y, T) => di(Y, T).
                r_3(X, Z, Y, T1, T2) : hit(X, Y, T1), hit(Z, Y, T2), prolog(nextD(T1, T2)) => -r_2(Z, Y, T2).
                
                f_1 :=> sh(bu, ge, 1).
                f_2 :=> sh(dl, ge, 3).
                
                """.trimIndent() + temporalDomain(5),
            ).let { solver ->
                isNessOriginalIntervention(solver, "[-sh(bu, ge, 1)]", "di(ge, 2)")
                isNessOriginalIntervention(solver, "[-sh(dl, ge, 3)]", "di(ge, 2)", false)
                isNessOriginalIntervention(solver, "[-sh(dl, ge, 3)]", "di(ge, 4)", false)

                isNessIntervention(solver, "[-sh(bu, ge, 1)]", "di(ge, 2)")
                isNessIntervention(solver, "[-sh(dl, ge, 3)]", "di(ge, 2)", false)
                isNessIntervention(solver, "[-sh(dl, ge, 3)]", "di(ge, 4)", false)
            }

    @Test
    fun doubleShootingTemporalAdvanced(): Unit =
        Arg2p
            .of(
                """
                r1(X, Y, T) : causesF(X, Y), happens(X, T) => holdsAt(Y, T).
                r2(X, Y, T) : causesE(X, Y), happens(X, T), prolog(next(T, T1)) => happens(Y, T1).
                r1_u(Y, T) : holdsAt(Y, T), prolog(next(T, T1)) => -r1(_, Y, T1).
                
                ri(Y, T) : holdsAt(Y, T), prolog(next(T, T1)) => holdsAt(Y, T1).
                
                d1 :=> causesE(sh(X, Y), hit(X, Y)).
                d2 :=> causesF(hit(X, Y), dead(Y)).
                
                f_1 :=> happens(sh(bu, ge), 1).
                f_2 :=> happens(sh(dl, ge), 3).
                
                """.trimIndent() + temporalDomain(5) + schemata,
            ).let { solver ->
                isNessOriginalIntervention(solver, "[-happens(sh(bu, ge), 1)]", "holdsAt(dead(ge), 5)")
                isNessOriginalIntervention(solver, "[-happens(sh(bu, ge), 3)]", "holdsAt(dead(ge), 5)", false)

                isNessIntervention(solver, "[-happens(sh(bu, ge), 1)]", "holdsAt(dead(ge), 5)")
                isNessIntervention(solver, "[-happens(sh(bu, ge), 3)]", "holdsAt(dead(ge), 5)", false)
            }

    @Test
    fun poisoningTemporal() {
        Arg2p
            .of(
                """
                r1(X, Y, T) : happens(X, T), causesF(X, Y, T)  => holdsAt(Y, T).
                r2(X, Y, T) : happens(X, T), causesE(X, Y, T), prolog(next(T, T1)) => happens(Y, T1).
                r3(Y, T) : holdsAt(Y, T), prolog(next(T, T1)) => -r1(_, Y, T1).
                r4(Y, T) : holdsAt(Y, T), prolog(next(T, T1)) => holdsAt(Y, T1).
                r5(Y, T) : happens(X, T), causesF(X, Y, T), prolog((complement(Y, CY), next(T1, T))) => -r4(CY, T1).
                s1(Y, T) : holdsAt(Y, T), prolog(complement(Y, CY))  -> -holdsAt(CY, T).

                complement(-X, X).
                complement(X, -X) :- X \= -_.

                c1 :=> causesF(empties, empty, T).
                c2 :=> causesF(poisons, poisoned, T).
                c3(T) : holdsAt(-empty, T), holdsAt(poisoned, T) => causesE(thirst, drinksPoison, T).
                c4(T) : holdsAt(empty, T) => causesE(thirst, dehydration, T).
                c5 :=> causesF(drinksPoison, dead, T).
                c6 :=> causesF(dehydration, dead, T).

                f_1 :=> holdsAt(-empty, 0).
                f_2 :=> happens(poisons, 1).
                f_3 :=> happens(empties, 2).
                f_4 :=> happens(thirst, 4).
                
                """.trimIndent() + temporalDomain(5) + schemata,
            ).let { solver ->
                isNessOriginalIntervention(solver, "[-happens(poisons, 1)]", "holdsAt(dead, 5)", false)
                isNessOriginalIntervention(solver, "[-happens(empties, 2)]", "holdsAt(dead, 5)")

                isNessIntervention(solver, "[-happens(poisons, 1)]", "holdsAt(dead, 5)", false)
                isNessIntervention(solver, "[-happens(empties, 2)]", "holdsAt(dead, 5)")
            }
    }

    // The same examples with the axioms of time (temporal): what holds is a period after an event (holdsAt(dead(ge), T)
    // for every T after 2), and no persistence argument is built for each time step, nor a horizon set
    @Test
    fun doubleShootingWithTheAxiomsOfTime(): Unit =
        Arg2p
            .of(
                """
                temporal.
                r2(X, Y, T) : causesE(X, Y), happens(X, T), prolog(T1 is T + 1) => happens(Y, T1).
                r1(X, Y) : causesF(X, Y) => initiates(X, Y).
                d1 :=> causesE(sh(X, Y), hit(X, Y)).
                d2 :=> causesF(hit(X, Y), dead(Y)).
                f_1 :=> happens(sh(bu, ge), 1).
                f_2 :=> happens(sh(dl, ge), 3).
                """.trimIndent(),
            ).let { solver ->
                isNessOriginalIntervention(solver, "[-happens(sh(bu, ge), 1)]", "holdsAt(dead(ge), 5)")
                isNessOriginalIntervention(solver, "[-happens(sh(dl, ge), 3)]", "holdsAt(dead(ge), 5)", false)
                isNessIntervention(solver, "[-happens(sh(bu, ge), 1)]", "holdsAt(dead(ge), 5)")
                isNessIntervention(solver, "[-happens(sh(dl, ge), 3)]", "holdsAt(dead(ge), 5)", false)
            }

    @Test
    fun poisoningWithTheAxiomsOfTime() {
        Arg2p
            .of(
                """
                temporal.
                r2(X, Y, T) : happens(X, T), causesE(X, Y, T), prolog(T1 is T + 1) => happens(Y, T1).
                r1(X, Y) : causesF(X, Y) => initiates(X, Y).
                c1 :=> causesF(empties, empty).
                c2 :=> causesF(poisons, poisoned).
                c3(T) : holdsAt(-empty, T), holdsAt(poisoned, T) => causesE(thirst, drinksPoison, T).
                c4(T) : holdsAt(empty, T) => causesE(thirst, dehydration, T).
                c5 :=> causesF(drinksPoison, dead).
                c6 :=> causesF(dehydration, dead).
                f_0 :=> happens(start, 0).
                f_1 :=> initiates(start, -empty).
                f_2 :=> happens(poisons, 1).
                f_3 :=> happens(empties, 2).
                f_4 :=> happens(thirst, 4).
                """.trimIndent(),
            ).let { solver ->
                isNessOriginalIntervention(solver, "[-happens(poisons, 1)]", "holdsAt(dead, 6)", false)
                isNessOriginalIntervention(solver, "[-happens(empties, 2)]", "holdsAt(dead, 6)")
                isNessIntervention(solver, "[-happens(poisons, 1)]", "holdsAt(dead, 6)", false)
                isNessIntervention(solver, "[-happens(empties, 2)]", "holdsAt(dead, 6)")
            }
    }
}
