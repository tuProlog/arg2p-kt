package it.unibo.tuprolog.argumentation.core.runtime.graph

import it.unibo.tuprolog.argumentation.InternalArg2pApi
import it.unibo.tuprolog.argumentation.core.model.terms.ArgumentTerm
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextSchema
import it.unibo.tuprolog.argumentation.core.runtime.context.contextLibrary
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.Record
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term

/**
 * A [GraphSegment] as terms, for a solver of another process: the identifiers of a session mean nothing there, so the
 * arguments are terms, those the facts refer to listed once in [refs] (the A of each `argument(A)`), the facts by their
 * positions in it. Merged by [mergeSegmentTerms], each argument identified in the session of the solver merging it.
 */
@InternalArg2pApi
class SegmentTerms(
    /** The arguments the segment refers to */
    val refs: List<Term>,
    /** The arguments saved, positions in [refs], in the order they were */
    val arguments: IntArray,
    /** The info of each of [arguments] (the I of its `info(I)`) */
    val infos: List<Term>,
    /** The supports, pairs of positions in [refs]: the supporter, the supported */
    val supports: IntArray,
    /** The attacks saved, and those removed: each its type and the positions of the attacker, the attacked, the one on */
    val attacks: List<AttackRef>,
    val removed: List<AttackRef>,
    /** The statements explored, in query mode */
    val explored: List<Term>,
) {
    /** Whether it holds nothing */
    val isEmpty: Boolean
        get() = arguments.isEmpty() && supports.isEmpty() && attacks.isEmpty() && removed.isEmpty() && explored.isEmpty()
}

/** An attack of a [SegmentTerms]: its type, and its arguments by their positions in the refs of the segment */
@InternalArg2pApi
class AttackRef(
    val type: String,
    val attacker: Int,
    val attacked: Int,
    val on: Int,
)

/** [segment], of the solver of the call, as terms (see [SegmentTerms]) */
@InternalArg2pApi
fun NativeCall.segmentTerms(segment: GraphSegment): SegmentTerms {
    val context = contextLibrary
    val refs = ArrayList<Term>()
    val positions = HashMap<Int, Int>()

    fun ref(id: Int): Int =
        positions.getOrPut(id) {
            refs += (context.term(id) as Struct)[0]
            refs.size - 1
        }

    val infoOf = HashMap<Int, Int>()
    for (info in segment.infos) infoOf[info.idAt(0)] = info.idAt(1)
    val arguments = IntArray(segment.arguments.size) { ref(segment.arguments[it]) }
    val infos = segment.arguments.map { id -> infoOf[id]?.let { (context.term(it) as Struct)[0] } ?: Atom.of("none") }
    val supports = IntArray(segment.supports.size * 2)
    segment.supports.forEachIndexed { i, fact ->
        supports[2 * i] = ref(fact.idAt(0))
        supports[2 * i + 1] = ref(fact.idAt(1))
    }

    fun attack(fact: Record): AttackRef {
        val term = termIn(fact) as Struct
        return AttackRef(term[0].toString(), ref(fact.idAt(1)), ref(fact.idAt(2)), ref(fact.idAt(3)))
    }
    val attacks = segment.attacks.map { attack(it) }
    val removed = segment.removed.map { attack(it) }
    val explored = segment.explored.map { (termIn(it) as Struct)[0] }
    return SegmentTerms(refs, arguments, infos, supports, attacks, removed, explored)
}

/**
 * [segment], of a solver of another process, merged into the store of the active branch as [mergeGraphSegment] merges a
 * segment of the session: the identifiers of the arguments new here, in the order of the segment
 */
@InternalArg2pApi
fun NativeCall.mergeSegmentTerms(
    segment: SegmentTerms,
    tag: String?,
    noted: (Record) -> Boolean = { true },
): IntArray {
    val context = contextLibrary
    val store = context.store()!!
    val saved = store.ids(ContextSchema.SAVED).copy()
    val ids = IntArray(segment.refs.size) { context.identifier(ContextSchema.argument(segment.refs[it])) }
    val added = ArrayList<Int>()
    segment.arguments.forEachIndexed { i, position ->
        val id = ids[position]
        if (!saved.add(id)) return@forEachIndexed
        store.assertA(context.record(id)!!)
        store.assertId(ContextSchema.SAVED, id)
        val info = segment.infos[i]
        if (info != Atom.of("none")) {
            val infoId = context.identifier(ContextSchema.info(info))
            store.assertA(Struct.of(ContextSchema.ARGUMENT_INFO, Integer.of(id), Integer.of(infoId)))
        }
        if (tag != null) {
            val conclusion = ArgumentTerm.parts(segment.refs[position])!!.conclusion
            if (noted(Record.of(conclusion))) store.assertA(Struct.of(ContextSchema.NEW_CONCLUSION, Atom.of(tag), conclusion))
        }
        added += id
    }
    for (i in 0 until segment.supports.size / 2) {
        val fact = Struct.of(ContextSchema.SUPPORT, Integer.of(ids[segment.supports[2 * i]]), Integer.of(ids[segment.supports[2 * i + 1]]))
        if (!store.holds(Record.of(fact))) store.assertA(fact)
    }

    fun attack(ref: AttackRef): Struct =
        Struct.of(
            ContextSchema.ATTACK,
            Atom.of(ref.type),
            Integer.of(ids[ref.attacker]),
            Integer.of(ids[ref.attacked]),
            Integer.of(ids[ref.on]),
        )
    for (ref in segment.attacks) {
        val fact = attack(ref)
        if (!store.holds(Record.of(fact))) store.assertA(fact)
    }
    for (ref in segment.removed) store.retractFact(Record.of(attack(ref)))
    return added.toIntArray()
}

/** The session of the solver of the call, if any: two solvers of one session read each other's segments as they are */
@InternalArg2pApi
fun NativeCall.graphSession(): Any? =
    contextLibrary
        .store()
        ?.encoding
        ?.session
