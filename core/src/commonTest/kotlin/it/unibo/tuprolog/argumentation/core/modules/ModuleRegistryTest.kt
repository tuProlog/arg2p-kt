package it.unibo.tuprolog.argumentation.core.modules

import it.unibo.tuprolog.argumentation.core.modules.graph.StructuredPartialModule
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.extractSignature
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

// The calls between the modules (Module::Goal, Module:::Goal) are checked on their theories, as the solvers parse them
class ModuleRegistryTest {
    // The predicates each module defines, by its identifier: its clauses and its native primitives
    private val definitions: Map<String, Set<Signature>> =
        ModuleRegistry.all.filterIsInstance<Loadable>().associate { module ->
            val library = (module as ArgLibrary).content()
            module.identifier() to library.clauses.map { it.head!!.extractSignature() }.toSet() + library.primitives.keys
        }

    // The module calls in a term, at any depth: those naming their module and goal (not Semantics::locality(L))
    private fun moduleCalls(term: Term): Sequence<Pair<String, Struct>> =
        when {
            term !is Struct -> emptySequence()
            term.arity == 2 && term.functor in setOf("::", ":::") && term[0].isAtom && term[1] is Struct ->
                sequenceOf(term[0].castToAtom().value to term[1].castToStruct()) + moduleCalls(term[1])
            else -> term.args.asSequence().flatMap { moduleCalls(it) }
        }

    // Each module call of each module: the module calling, the module called and the predicate
    private val calls: List<Triple<String, String, Signature>> =
        ModuleRegistry.all.flatMap { module ->
            module
                .content()
                .clauses
                .filterIsInstance<Clause>()
                .flatMap { moduleCalls(it.body).toList() }
                .map { (name, goal) -> Triple(module.alias, name, goal.extractSignature()) }
        }

    @Test
    fun theCallsAreFound() {
        assertTrue(Triple(StructuredPartialModule.alias, "queryExploration", Signature("explore", 1)) in calls)
    }

    @Test
    fun everyModuleCallIsDefined() {
        val undefined =
            calls
                // user:: calls reach the theory of the user, whatever it defines
                .filter { (_, name, _) -> name != "user" }
                .filter { (_, name, signature) -> definitions[name]?.contains(signature) != true }
                .map { (caller, name, signature) -> "$caller: $name::${signature.toIndicator()}" }
                .distinct()
        assertEquals(emptyList(), undefined)
    }

    // A native primitive shadows the clauses of its name: a module defines each of its predicates one way only
    @Test
    fun noPredicateIsBothClausesAndAPrimitive() {
        val both =
            ModuleRegistry.all.flatMap { module ->
                val clauses =
                    module
                        .content()
                        .clauses
                        .map { it.head!!.extractSignature() }
                        .toSet()
                module
                    .content()
                    .primitives.keys
                    .filter { it in clauses }
                    .map { "${module.alias}: ${it.toIndicator()}" }
            }
        assertEquals(emptyList(), both)
    }
}
