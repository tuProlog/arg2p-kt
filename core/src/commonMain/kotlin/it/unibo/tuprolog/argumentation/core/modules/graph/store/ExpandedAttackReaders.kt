package it.unibo.tuprolog.argumentation.core.modules.graph.store

import it.unibo.tuprolog.argumentation.core.context.ContextSchema
import it.unibo.tuprolog.argumentation.core.context.contextLibrary
import it.unibo.tuprolog.argumentation.core.graph.AttackIds
import it.unibo.tuprolog.argumentation.core.graph.ExpandedAttacks
import it.unibo.tuprolog.argumentation.core.graph.expandedAttacks
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.answersMatching
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.Signature

// The attacks to look at: those on the attacked argument or by the attacker if given, all of them otherwise
private fun ExpandedAttacks.candidates(
    attacker: Int?,
    attacked: Int?,
): List<AttackIds> =
    when {
        attacked != null -> byAttacked[attacked].orEmpty()
        attacker != null -> byAttacker[attacker].orEmpty()
        else -> attacks
    }

// Whether the attack may answer the arguments of the call, read as they are: only unifying them can tell otherwise
// when a variable is met twice with different identifiers, which is told here as well
private fun NativeCall.matches(
    attack: AttackIds,
    type: Int,
    attacker: Int,
    attacked: Int,
    on: Int,
): Boolean =
    (kind(type) != NativeCall.ATOM || atomName(type) == attack.type.castToAtom().value) &&
        idMatches(attacker, attack.attacker) &&
        idMatches(attacked, attack.attacked) &&
        idMatches(on, attack.on) &&
        (attacker != attacked || attack.attacker == attack.attacked) &&
        (attacker != on || attack.attacker == attack.on) &&
        (attacked != on || attack.attacked == attack.on)

private fun NativeCall.idMatches(
    w: Int,
    id: Int,
): Boolean = kind(w) == NativeCall.VAR || (kind(w) == NativeCall.INT && intValue(w) == id.toLong())

// The attacks from place [from] on answering the call, in turn: no alternative left after the last of them
private fun NativeCall.answerAttacks(
    attacks: List<AttackIds>,
    from: Int,
): Int {
    val type = arg(0)
    val attacker = arg(1)
    val attacked = arg(2)
    val on = arg(3)
    var i = from
    while (i < attacks.size && !matches(attacks[i], type, attacker, attacked, on)) i++
    if (i >= attacks.size) return NativeCall.FAIL
    var next = i + 1
    while (next < attacks.size && !matches(attacks[next], type, attacker, attacked, on)) next++
    if (next < attacks.size) alternatives { it.answerAttacks(attacks, next) }
    val attack = attacks[i]
    return if (unify(type, word(attack.type)) &&
        unify(attacker, int(attack.attacker)) &&
        unify(attacked, int(attack.attacked)) &&
        unify(on, int(attack.on))
    ) {
        NativeCall.CONTINUE
    } else {
        NativeCall.FAIL
    }
}

/** graphStore::expandedAttackById(?Type, ?IdAttacker, ?IdAttacked, ?IdOn): the attacks, saved or extended (see [ExpandedAttacks]) */
internal object ExpandedAttackById : AbstractNativePrimitive(Signature("expandedAttackById", 4)) {
    override fun call(call: NativeCall): Int {
        val attacker = call.arg(1)
        val attacked = call.arg(2)
        val attacks =
            call.expandedAttacks().candidates(
                attacker.takeIf { call.kind(it) == NativeCall.INT }?.let { call.intValue(it).toInt() },
                attacked.takeIf { call.kind(it) == NativeCall.INT }?.let { call.intValue(it).toInt() },
            )
        return call.answerAttacks(attacks, 0)
    }
}

// The arguments of a reader, from [from] on, as terms, the variables among them null
private fun NativeCall.readerArguments(from: Int): List<Term?> =
    (from until arity).map { i -> arg(i).let { w -> if (kind(w) == NativeCall.VAR) null else term(w) } }

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
            argument?.takeIf { it.isGround }?.let { context.knownIdentifier(ContextSchema.argument(it)) }
        }
    val argument = { id: Int -> context.term(id)?.castToStruct()?.get(0) }
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
