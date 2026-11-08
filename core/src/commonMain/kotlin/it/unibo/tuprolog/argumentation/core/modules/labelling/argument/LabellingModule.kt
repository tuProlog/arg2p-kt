package it.unibo.tuprolog.argumentation.core.modules.labelling.argument

import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.labelling.argument.AdmissibleWithin
import it.unibo.tuprolog.argumentation.core.primitives.labelling.argument.MaximalAdmissibleSubset
import it.unibo.tuprolog.argumentation.core.primitives.labelling.argument.NewLabelling
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * What the semantics share (module `labelling`, no semantics itself): the arguments to label, the labelling of a set of
 * arguments IN, the labellings of a semantics refined into those of another, and whether a component is defended.
 */
object LabellingModule : PrologModule("prolog.argumentation.graph.labelling.support", "labelling") {
    override val primitives: Map<Signature, Primitive>
        get() = listOf(NewLabelling, AdmissibleWithin, MaximalAdmissibleSubset).associateBy { it.signature }

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! candidates(-Args, -Forced)
        %  The arguments to label, and those among them already IN (kept IN)
        candidates(Args, Forced) :-
            graphStore::argumentIds(Args),
            findall(A, (member(A, Args), graphStore::argumentLabelById(in, A)), Forced).

        %! labelSet(+Branch, +Set)
        %  In a new branch of Branch, Set IN, the arguments it attacks OUT, the rest UND;
        %  the branch is released on backtracking
        labelSet(Branch, Set) :-
            newLabelling(Branch, Set),
            context_active(Labelling),
            (
                true
            ;
                context_checkout(Branch),
                context_drop(Labelling),
                fail
            ).

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
            labelSet(Branch, In).

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
            labelSet(Branch, Maximal).
        """.trimIndent()
}
