package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.core.Var
import kotlin.jvm.JvmField

/**
 * The state of the native call at hand (see [NativeCall]): its arity, the variables its terms gave the cells, and the cells
 * its words gave the variables of terms that were not cells already. Within one call the same cell reads as the same
 * variable, and the same variable writes to the same cell. A nested run the call starts holds them ([hold]), as roots of
 * the collector, and gives them back after it ([restore]).
 */
internal class NativeContext {
    @JvmField var arity = 0

    // The variables the terms of the call gave the cells
    @JvmField var names: HashMap<Int, Var>? = null

    // The cells the words of the call gave the variables of its terms (fresh ones of a clause copied, say)
    @JvmField var fresh: HashMap<Var, Int>? = null

    // How many of [names] are in [fresh] already
    private var merged = 0

    // Those of the native calls a nested run is under
    private val heldNames = ArrayList<HashMap<Int, Var>>()
    private val heldFresh = ArrayList<HashMap<Var, Int>>()

    /** A native call of [arity] arguments begins: nothing named yet */
    fun begin(arity: Int) {
        this.arity = arity
        names = null
        fresh = null
        merged = 0
    }

    /** A native call resumed by backtracking: the variables it named, of the cells still there (below [top]) */
    fun resume(
        arity: Int,
        names: HashMap<Int, Var>?,
        top: Int,
    ) {
        begin(arity)
        this.names = names?.let { below(it, top) }
    }

    /** The variables of the cells, for a term read of the heap */
    fun namesOfCells(): HashMap<Int, Var> = names ?: HashMap<Int, Var>().also { names = it }

    /** The cells of the variables, for a term written on the heap: those the call named first */
    fun cellsOfVariables(): HashMap<Var, Int> {
        val names = names
        val cells = fresh ?: HashMap<Var, Int>().also { fresh = it }
        if (names != null && names.size != merged) {
            for ((cell, variable) in names) cells[variable] = cell
            merged = names.size
        }
        return cells
    }

    /** The maps of the call held across a nested run, as roots */
    fun hold() {
        names?.let { heldNames.add(it) }
        fresh?.let { heldFresh.add(it) }
    }

    /** The maps [hold] held for the call of [names] and [fresh] released */
    fun release(
        names: HashMap<Int, Var>?,
        fresh: HashMap<Var, Int>?,
    ) {
        if (names != null) heldNames.removeAt(heldNames.size - 1)
        if (fresh != null) heldFresh.removeAt(heldFresh.size - 1)
    }

    /** The call at hand again after a nested run: its state as it was */
    fun restore(
        arity: Int,
        names: HashMap<Int, Var>?,
        fresh: HashMap<Var, Int>?,
    ) {
        this.arity = arity
        this.names = names
        this.fresh = fresh
        // (merged again: a collection in the nested run may have dropped names, and more may come to the same number)
        merged = 0
    }

    /** The maps of the call at hand and of those held, to the collector: those of names to [eachNames], of fresh cells to [eachFresh] */
    fun forEachMap(
        eachNames: (HashMap<Int, Var>) -> Unit,
        eachFresh: (HashMap<Var, Int>) -> Unit,
    ) {
        names?.let(eachNames)
        heldNames.forEach(eachNames)
        fresh?.let(eachFresh)
        heldFresh.forEach(eachFresh)
        merged = 0
    }

    private fun below(
        names: HashMap<Int, Var>,
        top: Int,
    ): HashMap<Int, Var> {
        val below = HashMap<Int, Var>(names.size * 2)
        for ((cell, variable) in names) if (cell < top) below[cell] = variable
        return below
    }
}
