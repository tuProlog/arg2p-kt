package it.unibo.tuprolog.argumentation.core.primitives.graph

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.answersMatching
import it.unibo.tuprolog.argumentation.core.runtime.context.contextLibrary
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.Signature

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

// The arguments of a reader, from [from] on, as terms, the variables among them null
private fun NativeCall.readerArguments(from: Int): List<Term?> =
    (from until arity).map { i -> arg(i).let { w -> if (kind(w) == NativeCall.VAR) null else term(w) } }

/** graphStore::expandedAttackById(?Type, ?IdAttacker, ?IdAttacked, ?IdOn): the attacks, saved or extended (see [ExpandedAttacks]), by identifiers */
internal object ExpandedAttackById : AbstractNativePrimitive(Signature("expandedAttackById", 4)) {
    override fun call(call: NativeCall): Int {
        val (_, attacker, attacked, _) = call.readerArguments(0)
        val attacks = call.expandedAttacks().candidates(attacker, attacked)
        return call.answersMatching(0, attacks.asSequence().map { listOf(it.type, it.attacker, it.attacked, it.on) }.iterator())
    }
}

// The attacks, saved or extended, as arguments rather than identifiers (those of argument/1 terms, see context_id/2),
// in the given branch (the active one if null), unified with the arguments of the call from [from] on
private fun NativeCall.attacksByTerms(
    from: Int,
    branch: Int?,
): Int {
    val (_, attacker, attacked, _) = readerArguments(from)
    val context = contextLibrary
    // an argument saved has an identifier; any other is unified with each attack, as an instance of an argument saved
    // with variables may be (the variables of a conclusion bound by an attack)
    val (idAttacker, idAttacked) =
        listOf(attacker, attacked).map { argument ->
            argument?.takeIf { it.isGround }?.let { context.knownIdentifier(Struct.of("argument", it)) }?.let { Integer.of(it) }
        }
    val argument = { id: Term ->
        id
            .asInteger()
            ?.let { context.term(it.intValue.toInt()) }
            ?.castToStruct()
            ?.get(0)
    }
    val attacks = expandedAttacks(branch).candidates(idAttacker, idAttacked)
    return answersMatching(
        from,
        attacks
            .asSequence()
            .mapNotNull { attack ->
                listOf(attack.attacker, attack.attacked, attack.on).map { argument(it) ?: return@mapNotNull null }.let {
                    listOf(attack.type) +
                        it
                }
            }.iterator(),
    )
}

/** graphStore::expandedAttack(?Type, ?Attacker, ?Attacked, ?On): the attacks, saved or extended (see [ExpandedAttacks]) */
internal object ExpandedAttack : AbstractNativePrimitive(Signature("expandedAttack", 4)) {
    override fun call(call: NativeCall): Int = call.attacksByTerms(0, null)
}

/** graphStore::expandedAttackIn(+Branch, ?Type, ?Attacker, ?Attacked, ?On): as expandedAttack/4, in the given branch */
internal object ExpandedAttackIn : AbstractNativePrimitive(Signature("expandedAttackIn", 5)) {
    override fun call(call: NativeCall): Int {
        val branch = call.arg(0)
        if (call.kind(branch) != NativeCall.INT) return NativeCall.FAIL
        return call.attacksByTerms(1, call.intValue(branch).toInt())
    }
}
