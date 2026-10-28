package it.unibo.tuprolog.argumentation.core.primitives.graph

import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.argumentation.core.runtime.context.contextLibrary
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

// The answers of a reader: each candidate unified with the arguments of the request, its variables renamed, as
// those of a clause read from the context are
private fun Solve.Request<ExecutionContext>.answers(
    arguments: List<Term>,
    candidates: Sequence<List<Term>>,
): Sequence<Solve.Response> {
    val goal = Struct.of("t", arguments)
    return candidates.mapNotNull { values ->
        Arg2pUnificator.mgu(goal, Struct.of("t", values).freshCopy()).takeIf { it.isSuccess }?.let { replyWith(it) }
    }
}

// The attacks to look at: those on the attacked argument or by the attacker if given, all of them otherwise
private fun ExpandedAttacks.candidates(
    attacker: Term?,
    attacked: Term?,
): List<AttackIds> =
    when {
        attacked != null -> byAttacked[attacked].orEmpty()
        attacker != null -> byAttacker[attacker].orEmpty()
        else -> attacks
    }

/** graphStore::expandedAttackById(?Type, ?IdAttacker, ?IdAttacked, ?IdOn): the attacks, saved or extended (see [ExpandedAttacks]), by identifiers */
internal object ExpandedAttackById : PrimitiveWithSignature {
    override val signature = Signature("expandedAttackById", 4)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val (_, attacker, attacked, _) = request.arguments
        val attacks = request.expandedAttacks().candidates(attacker.takeUnless { it is Var }, attacked.takeUnless { it is Var })
        return request.answers(request.arguments, attacks.asSequence().map { listOf(it.type, it.attacker, it.attacked, it.on) })
    }
}

// The attacks, saved or extended, as arguments rather than identifiers (those of argument/1 terms, see context_id/2),
// in the given branch (the active one if null)
private fun Solve.Request<ExecutionContext>.attacksByTerms(
    arguments: List<Term>,
    branch: Int?,
): Sequence<Solve.Response> {
    val (_, attacker, attacked, _) = arguments
    val context = contextLibrary
    // an argument saved has an identifier; any other is unified with each attack, as an instance of an argument saved
    // with variables may be (the variables of a conclusion bound by an attack)
    val (idAttacker, idAttacked) =
        listOf(attacker, attacked).map { argument ->
            argument.takeIf { it.isGround }?.let { context.knownIdentifier(Struct.of("argument", it)) }?.let { Integer.of(it) }
        }
    val argument = { id: Term ->
        id
            .asInteger()
            ?.let { context.term(it.intValue.toInt()) }
            ?.castToStruct()
            ?.get(0)
    }
    val attacks = expandedAttacks(branch).candidates(idAttacker, idAttacked)
    return answers(
        arguments,
        attacks.asSequence().mapNotNull { attack ->
            listOf(attack.attacker, attack.attacked, attack.on).map { argument(it) ?: return@mapNotNull null }.let {
                listOf(attack.type) +
                    it
            }
        },
    )
}

/** graphStore::expandedAttack(?Type, ?Attacker, ?Attacked, ?On): the attacks, saved or extended (see [ExpandedAttacks]) */
internal object ExpandedAttack : PrimitiveWithSignature {
    override val signature = Signature("expandedAttack", 4)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> = request.attacksByTerms(request.arguments, null)
}

/** graphStore::expandedAttackIn(+Branch, ?Type, ?Attacker, ?Attacked, ?On): as expandedAttack/4, in the given branch */
internal object ExpandedAttackIn : PrimitiveWithSignature {
    override val signature = Signature("expandedAttackIn", 5)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val branch =
            request.arguments[0]
                .asInteger()
                ?.intValue
                ?.toInt() ?: return emptySequence()
        return request.attacksByTerms(request.arguments.drop(1), branch)
    }
}
