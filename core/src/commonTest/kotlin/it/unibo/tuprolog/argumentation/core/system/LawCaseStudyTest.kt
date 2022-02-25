package it.unibo.tuprolog.argumentation.core.system

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.TestingUtils
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.libs.basic.FlagsBuilder
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.classic.ClassicSolverFactory
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import kotlin.test.Test
import kotlin.time.DurationUnit
import kotlin.time.ExperimentalTime
import kotlin.time.measureTime

class LawCaseStudyTest {

    private val baseTheory: String =
        """
        art1a: evaluate(Ev), hasAgent(Ev,X), licensee(X), hasTheme(Ev,P), product(P) => o(-evaluate(Ev)).
        
        art1b: evaluate(Ev), hasAgent(Ev,X), licensee(X), hasTheme(Ev,P), product(P), isLicenceOf(L,P), licence(L), hasTheme(Eg,L), hasAgent(Eg,Y), licensor(Y), grant(Eg), rexist(Eg), hasReceiver(Eg,X) => p(evaluate(Ev)).
        sup(art1b, art1a).
        
        art2aPart1: evaluate(Ev), rexist(Ev), hasResult(Ev,R), result(R), publish(Ep), hasAgent(Ep,X), licensee(X), hasTheme(Ep,R) => condition_2(Ep,X,R).
        art2aPart2: condition_2(Ep,X,R) => o(-publish(Ep)).
        
        art2b: approve(Ea), rexist(Ea), hasTheme(Ea,Ep), hasAgent(Ea,Y), licensor(Y), publish(Ep), hasAgent(Ep,X), licensee(X), hasTheme(Ep,R), result(R), hasResult(Ev,R), evaluate(Ev), rexist(Ev) => p(publish(Ep)).
        sup(art2b, art2aPart2).
        
        art2cPart1: condition_2(Ep,X,R), o(-publish(Ep)), rexist(Ep) => o(remove(ca(Ep,X,R))).
        art2cPart2: condition_2(Ep,X,R), o(-publish(Ep)), rexist(Ep) => remove(ca(Ep,X,R)).
        art2cPart3: condition_2(Ep,X,R), o(-publish(Ep)), rexist(Ep) => hasTheme(ca(Ep,X,R),R).
        art2cPart4: condition_2(Ep,X,R), o(-publish(Ep)), rexist(Ep) => hasAgent(ca(Ep,X,R),X).
        art2e: condition_2(Ep,X,R), o(-publish(Ep)), rexist(Ep) => compensate(ca(Ep,X,R),Ep).
        
        art3a: publish(Ep), hasAgent(Ep,X), licensee(X), hasTheme(Ep,C), comment(C), isCommentOf(C,Ev), evaluate(Ev), rexist(Ev) => o(-publish(Ep)).
        art3b: publish(Ep), hasAgent(Ep,X), licensee(X), hasTheme(Ep,C), comment(C), isCommentOf(C,Ev), evaluate(Ev), rexist(Ev), hasResult(Ev,R), hasTheme(Epr,R), hasAgent(Epr,X), publish(Epr), p(publish(Epr)) => p(publish(Ep)).
        sup(art3b, art3a).
        
        art4a: publish(Ep), hasAgent(Ep,X), licensee(X), hasTheme(Ep,R), result(R), hasResult(Ev,R), evaluate(Ev), rexist(Ev), hasTheme(Ec,Ev), commission(Ec), rexist(Ec) => o(publish(Ep)).
        sup(art4a, art2aPart2).
        
        ccRuleComp1: remove(ca(Ep,X,R)), hasTheme(ca(Ep,X,R),R), hasAgent(ca(Ep,X,R),X), rexist(Er), remove(Er), hasTheme(Er,R), hasAgent(Er,X) => rexist(ca(Ep,X,R)).
        ccRuleComp2: compensate(Y,X), rexist(Y) => compensated(X).
        
        ccRuleEp1: o(-publish(Ep)), rexist(Ep), ~(compensated(Ep)) => referTo(viol(Ep),Ep).
        ccRuleEp2: o(publish(Ep)), ~(rexist(Ep)) => referTo(viol(Ep),Ep).
        ccRuleEr: o(remove(Er)) => referTo(viol(Er),Er).
        
        ruleViolation: referTo(viol(X),X) => violation(viol(X)).
        
        f1 :-> evaluate(ev).
        f2 :-> hasAgent(ev,x).
        f3 :-> licensee(x).
        f4 :-> hasTheme(ev,p).
        f5 :-> product(p).
        f6 :-> isLicenceOf(l,p).
        f7 :-> licence(l).
        f8 :-> grant(eg).
        f9 :-> rexist(eg).
        f10 :-> hasTheme(eg,l).
        f11 :-> hasAgent(eg,y).
        f12 :-> licensor(y).
        f13 :-> hasReceiver(eg,x).
        f14 :-> rexist(ev).
        f15 :-> hasResult(ev,r).
        f16 :-> result(r).
        f17 :-> publish(epr). 
        f18 :-> hasAgent(epr,x). 
        f19 :-> hasTheme(epr,r).
        %f20 :-> approve(ea).
        f21 :-> rexist(ea).
        f22 :-> hasTheme(ea,epr).
        f23 :-> hasAgent(ea,y).
        f24 :-> rexist(epr).
        f25 :-> publish(epc). 
        f26 :-> rexist(epc). 
        f27 :-> hasAgent(epc,x). 
        f28 :-> hasTheme(epc,c).
        f29 :-> comment(c).
        f30 :-> isCommentOf(c,ev).
        f31 :-> commission(ec).
        %f32 :-> rexist(ec).
        f33 :-> hasTheme(ec,ev).
        %f34 :-> rexist(er).
        f35 :-> remove(er).
        f36 :-> hasTheme(er,r).
        f37 :-> hasAgent(er,x).
        
        graphBuildMode(standard_af).
        statementLabellingMode(statement).
        argumentLabellingMode(grounded).
        orderingPrinciple(last).
        orderingComparator(elitist).
        graphExtension(standardPref).
        
        """.trimIndent()

    private val structuredTheory = baseTheory + "queryMode."

    private fun evaluateBoth(query: String, argsIn: String, argsOut: String, argsUnd: String) {
        TestingUtils.answerQuery(baseTheory, query, argsIn, argsOut, argsUnd)
        TestingUtils.answerQuery(structuredTheory, query, argsIn, argsOut, argsUnd)
    }

    @Test
    @ExperimentalTime
    fun abstractResolutionSpeedTest() {
        val time = measureTime {
            TestingUtils.answerQuery(
                baseTheory,
                "violation(X)",
                "[violation(viol(ca(epr, x, r))), violation(viol(epr)), violation(viol(epc))]",
                "[]",
                "[]"
            )
        }

        println(time.toDouble(DurationUnit.SECONDS))
    }

    @Test
    @ExperimentalTime
    fun structuredResolutionSpeedTest() {
        val time = measureTime {
            TestingUtils.answerQuery(
                structuredTheory,
                "violation(X)",
                "[violation(viol(ca(epr, x, r))), violation(viol(epr)), violation(viol(epc))]",
                "[]",
                "[]"
            )
        }

        println(time.toDouble(DurationUnit.SECONDS))
    }

    @Test
    @ExperimentalTime
    fun structuredTest() {
        val arg2p = Arg2pSolver.default()
        val solver = ClassicSolverFactory.mutableSolverWithDefaultBuiltins(
            otherLibraries = arg2p.to2pLibraries().plus(FlagsBuilder().create().content()),
            staticKb = Theory.parse(baseTheory, arg2p.operators())
        )
        val time = measureTime {
            arg2pScope {
                solver.solve("parser" call "convertAllRules"(`_`)).first()
            }
        }
        val time2 = measureTime {
            arg2pScope {
                val violation = solver.solve("structured" call "computeStatementAcceptance"("violation"(W), X, `_`, `_`))
                    .filter { it.isYes }
                    .map { it.substitution[X]!!.castToList() }
                    .first()
                val obligation = solver.solve("structured" call "computeStatementAcceptance"("o"("remove"(W)), X, `_`, `_`))
                    .filter { it.isYes }
                    .map { it.substitution[X]!!.castToList() }
                    .first()
                println(violation)
                println(obligation)
                // solver.solve("structured" call "computeStatementAcceptance"(Struct.parse("violation(viol(epr))", arg2p.operators()), X, Y, Z)).first()
                // solver.solve("structured" call "computeStatementAcceptance"(Struct.parse("violation(viol(ca(epr, x, r)))", arg2p.operators()), X, Y, Z)).first()
            }
        }

        println(time.toDouble(DurationUnit.SECONDS))
        println(time2.toDouble(DurationUnit.SECONDS))
    }

}
