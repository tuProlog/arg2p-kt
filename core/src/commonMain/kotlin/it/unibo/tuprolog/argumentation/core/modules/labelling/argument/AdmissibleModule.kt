package it.unibo.tuprolog.argumentation.core.modules.labelling.argument

import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.labelling.argument.AdmissibleSet
import it.unibo.tuprolog.argumentation.core.primitives.labelling.argument.MaximalAdmissibleSubset
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The admissible labellings (module `admissible`, an argumentLabellingMode): the conflict-free ones in which
 * every attacker of an IN argument is attacked by an IN argument.
 */
object AdmissibleModule : PrologModule("prolog.argumentation.graph.labelling.admissible", "admissible") {
    override val primitives: Map<Signature, Primitive>
        get() =
            (listOf(AdmissibleSet(0), AdmissibleSet(1)).associateBy { it.signature }) +
                (MaximalAdmissibleSubset.signature to MaximalAdmissibleSubset)

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! minimalUndOf(+Semantics, +Branch)
        %  The labellings of Semantics with a minimal set of UND arguments (semi-stable for preferred, stage for naive),
        %  each labelled in a branch of Branch.
        minimalUndOf(Semantics, Branch) :-
            findall(Und-In, (
                Semantics:::argumentLabelling,
                graphStore::labelledIds(und, Und),
                graphStore::labelledIds(in, In)
            ), Labellings),
            utils::minimalBySet(Labellings, Minimal),
            member(In, Minimal),
            conflictfree::labelSet(Branch, In).

        %! maximalAdmissibleCommonOf(+Semantics, +Branch)
        %  The labelling with IN the maximal admissible subset of the intersection of the labellings of Semantics
        %  (ideal for preferred, eager for semistable), in a branch of Branch.
        maximalAdmissibleCommonOf(Semantics, Branch) :-
            findall(In, (
                Semantics:::argumentLabelling,
                graphStore::labelledIds(in, In)
            ), Labellings),
            utils::intersectionOf(Labellings, Common),
            maximalAdmissibleSubset(Common, Maximal),
            conflictfree::labelSet(Branch, Maximal).

        %! locality(-Locality)
        %  Query mode: the label of an argument only depends on the arguments it depends on (directionality)
        locality(directional).

        %! argumentLabelling
        %  Each admissible labelling in turn, left as the active context.
        argumentLabelling :-
            context_active(Branch),
            conflictfree::candidates(Args, Forced),
            conflictfree::conflictFreeSubset(Args, Forced, Set),
            admissibleSet(Set),
            conflictfree::labelSet(Branch, Set).
        """.trimIndent()
}
