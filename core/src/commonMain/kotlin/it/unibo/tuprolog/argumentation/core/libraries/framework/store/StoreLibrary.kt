package it.unibo.tuprolog.argumentation.core.libraries.framework.store

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.primitives.ExpandedAttack
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.primitives.ExpandedAttackById
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.primitives.ExpandedAttackIn
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.primitives.GraphAttackSaved
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.primitives.GraphBegin
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.primitives.GraphExplore
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.primitives.GraphOwn
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.primitives.GraphRemoved
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.primitives.GraphSettle
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.primitives.GraphShare
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.primitives.GraphStep
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.primitives.GraphSync
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.primitives.RuleToClause
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.primitives.RulesAndInfo
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.primitives.Sccs
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.prolog.THEORY
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The argumentation graph kept in the active context, and its labels (module `store`): the only module reading
 * and writing them in the context, and the terms they are made of: the argument term (newArgument/6 and its parts,
 * claim/2, conclusion/2, ..., read both ways; see [it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.ArgumentTerm]),
 * the labels as facts (see [it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.LabelTerm]), the attacks and supports.
 *
 * An argument is kept once, as the term of its identifier (`context_id`, `context_term`): `argument/1` holds that record,
 * looked up by the claim inside it, and the rest of the graph refers to it by the identifier. The context keeps:
 * - `arg(Id)`, the arguments of the graph the labellings label;
 * - `newConc(Tag, Conclusion)`, the conclusions of the arguments saved under a tag (the round of the graph builder);
 * - the graph by these identifiers: `argInfo(Id, InfoId)`, `supportId(IdA, IdB)`, `attackId(Type, IdA, IdB, IdOn)`; InfoId
 *   identifies the info of the argument (see the module argument), what the preferences weigh;
 * - the labels of the arguments and of the statements (see the module label): `in(A)` shares the record of `argument(A)`.
 *
 * `support/2` and `attack/4`, looked up in the context, are answered from the identifiers, each argument read on its own.
 *
 * Only the attacks found are saved: an attack on an argument is an attack on every argument built on it as well, and
 * the attacks are read with those (attack/4, attackById/4, attacksById/2, attackIn/5, and the attack graph of the
 * native labellings), extended once until the attacks or the supports change (see ExpandedAttacks).
 */
object StoreLibrary : PrologLibrary("store") {
    override val primitives: Map<Signature, Primitive>
        get() =
            listOf(
                ExpandedAttack,
                ExpandedAttackById,
                ExpandedAttackIn,
                GraphShare,
                GraphBegin,
                GraphSettle,
                GraphOwn,
                GraphExplore,
                GraphStep,
                GraphRemoved,
                GraphAttackSaved,
                GraphSync,
                RulesAndInfo,
                RuleToClause,
                Sccs(1),
                Sccs(2),
            ).associateBy {
                it.signature
            }

    override val prologRawTheory: String = THEORY
}
