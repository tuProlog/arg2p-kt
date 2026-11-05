package it.unibo.tuprolog.argumentation.core.primitives.graph

import it.unibo.tuprolog.argumentation.InternalArg2pApi
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.model.terms.ArgumentTerm
import it.unibo.tuprolog.argumentation.core.runtime.context.contextLibrary
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term

/**
 * [arguments], ground, saved as `graphStore::saveArguments(Tag, Arguments)` saves them, straight in the store of the active
 * branch: the identifier of each. What the catalog of the parallel solver does with all the workers built, which on the heap,
 * and with a resolution for each, takes more than building them.
 */
@InternalArg2pApi
fun NativeCall.saveBuiltArguments(
    tag: String,
    arguments: Collection<Term>,
): IntArray {
    val context = contextLibrary
    val store = context.store()!!
    val saved = store.ids("arg").copy()
    val noted = Atom.of(tag)
    val ids = IntArray(arguments.size)
    arguments.forEachIndexed { i, argument ->
        val id = context.identifier(Struct.of("argument", argument))
        ids[i] = id
        if (saved.add(id)) {
            val parts = ArgumentTerm.parts(argument)!!
            store.assertA(context.record(id)!!)
            store.assertId("arg", id)
            val info = ArgumentTerm.info(parts.lastDefeasibleRules, parts.defeasibleRules, parts.defeasiblePremises)
            val infoId = context.identifier(Struct.of("info", info))
            store.assertA(Struct.of("argInfo", Integer.of(id), Integer.of(infoId)))
            store.assertA(Struct.of("newConc", noted, parts.conclusion))
        }
    }
    return ids
}
