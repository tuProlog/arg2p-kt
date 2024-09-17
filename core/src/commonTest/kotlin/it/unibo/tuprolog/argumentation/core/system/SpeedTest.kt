package it.unibo.tuprolog.argumentation.core.system

import it.unibo.tuprolog.argumentation.core.TestingUtils
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.dsl.logicProgramming
import it.unibo.tuprolog.solve.assertSolutionEquals
import it.unibo.tuprolog.solve.flags.LastCallOptimization
import it.unibo.tuprolog.solve.yes
import kotlin.test.Ignore
import kotlin.test.Test
import kotlin.time.DurationUnit
import kotlin.time.ExperimentalTime
import kotlin.time.measureTime

class SpeedTest {

    private val baseTheory: String =
        """
        r1 : on_road(V),
            traffic_light(V, red) => o(stop(V)).
        r2 : on_road(V),
            traffic_light(V,green) => p(-stop(V)).

        r3 : on_road(V), authorised_vehicle(V),
            acoustic_signals(V, on),
            light_signals(V, on) => emergency(V).
        r4 : on_road(V), emergency(V),
            traffic_light(V, red) => p(-stop(V)).

        r5 : on_road(V), emergency(V1),
            prolog(V \== V1),
            traffic_light(V, green) => o(stop(V)).

        sup(r4, r1).
        sup(r5, r2).

        f0 :-> authorised_vehicle(ambulance).

        f1 :-> on_road(car).
        f2 :-> on_road(ambulance).
        f3 :-> on_road(pedestrian).

        f4 :=> acoustic_signals(ambulance, on).
        f5 :=> light_signals(ambulance, on).
        f6 :=> traffic_light(ambulance, red).
        f7 :=> traffic_light(car, red).
        f8 :=> traffic_light(pedestrian, green).

        r6 : -stop(V),
            p(-stop(V)) => legitimate_cross(V).
        r7 : -stop(V),
            o(stop(V)) => -legitimate_cross(V).

        r8 : harms(P1, P2),
            -careful(P1) => responsible(P1).
        r9 : harms(P1, P2),
            -careful(P2) => responsible(P2).

        r10 : -legitimate_cross(V), 
            user(P, V) => -careful(P).
        r11 : high_speed(V), user(P, V)  => -careful(P).
        r12 : legitimate_cross(V),
            -high_speed(V), user(P, V)  => careful(P).

        r13 : witness(X),
            claim(X, low_speed(V)) => -high_speed(V).
        r14 : witness(X),
            claim(X, high_speed(V)) => high_speed(V).

        bp(careful(P)).

        f9 :-> user(pino, pedestrian).
        f10 :-> user(lisa, ambulance).
        f11 :-> -stop(ambulance).
        f12 :-> -stop(pedestrian).
        f13 :-> harms(lisa, pino).
        f14 :-> witness(chris).
        f15 :-> witness(john).
        f16 :=> claim(chris, low_speed(ambulance)).
        f17 :=> claim(john, high_speed(ambulance)).

        r15 : harms(P1, P2), user(P1, V),
            -working(V), manufacturer(M, V),
            -defect_free(V) => responsible(M).

        r16 : tried_to_brake(P), user(P, V),
            -working(V) => careful(P).

        r17 : mechanic(M),
            claim(M, defect(V)) => -working(V).

        r18 : -working(V), new(V) => -defect_free(V).
        r19 : production_manager(P),
            claim(P, test_ok(V)) => defect_free(V).
        r20 : test_doc_ok(V) => undercut(r18).

        sup(r16, r11).
        bp(defect_free(V)).

        f19 :-> manufacturer(demers , ambulance).
        f20 :=> tried_to_brake(lisa).
        f21 :-> mechanic(paul).
        f22 :=> claim(paul, defect(ambulance)).
        f23 :-> new(ambulance).
        f24 :-> production_manager(mike).
        f25 :=> claim(mike, test_ok(ambulance)).
        
        graphBuildMode(standard_af).
        statementLabellingMode(statement).
        argumentLabellingMode(grounded).
        orderingPrinciple(last).
        orderingComparator(elitist).
        graphExtension(standardPref).
        
        """.trimIndent()

    private val structuredTheory = baseTheory + "queryMode."

    @Test
    @ExperimentalTime
    @Ignore
    fun structuredResolutionSpeedTestNoLastCall() {
        val time = measureTime {
            logicProgramming {
                TestingUtils.solverWithTheory(structuredTheory).also { solver ->

                    val query = set_flag(LastCallOptimization.name, LastCallOptimization.OFF) and
                        current_flag(LastCallOptimization.name, V)

                    assertSolutionEquals(
                        listOf(query.yes(V to LastCallOptimization.OFF)),
                        solver.solve(query).toList()
                    )

                    TestingUtils.testGoalNoBacktracking(
                        "answerQuery"(Struct.parse("responsible(X)"), "StatIn", "StatOut", "StatUnd"),
                        solver
                    ) {
                        it.yes(
                            "StatIn" to Struct.parse("[responsible(pino)]"),
                            "StatOut" to Struct.parse("[responsible(lisa)]"),
                            "StatUnd" to Struct.parse("[responsible(demers)]")
                        )
                    }
                }
            }
        }

        println(time.toDouble(DurationUnit.SECONDS))
    }

    @Test
    @ExperimentalTime
    fun structuredResolutionSpeedLastCall() {
        val time = measureTime {
            logicProgramming {
                TestingUtils.solverWithTheory(structuredTheory).also { solver ->

                    val query = current_flag(LastCallOptimization.name, V)

                    assertSolutionEquals(
                        listOf(query.yes(V to LastCallOptimization.ON)),
                        solver.solve(query).toList()
                    )

                    TestingUtils.testGoalNoBacktracking(
                        "answerQuery"(Struct.parse("responsible(X)"), "StatIn", "StatOut", "StatUnd"),
                        solver
                    ) {
                        it.yes(
                            "StatIn" to Struct.parse("[responsible(pino)]"),
                            "StatOut" to Struct.parse("[responsible(lisa)]"),
                            "StatUnd" to Struct.parse("[responsible(demers)]")
                        )
                    }
                }
            }
        }

        println(time.toDouble(DurationUnit.SECONDS))
    }
}
