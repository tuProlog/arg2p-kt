package it.unibo.tuprolog.argumentation.core.modules.utils

import it.unibo.tuprolog.argumentation.engine.term.renamedCopy
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.TermComparator

internal fun List.sortedTerms(): kotlin.collections.List<Term> = toList().sortedWith(TermComparator.DefaultComparator)

// Standard order of terms, duplicates removed, and variables renamed apart as in setof/3
internal fun List.distinctSortedTerms(): kotlin.collections.List<Term> =
    sortedTerms()
        .fold(mutableListOf<Term>()) { acc, term ->
            acc.also { if (it.isEmpty() || TermComparator.DefaultComparator.compare(it.last(), term) != 0) it.add(term) }
        }.map { it.renamedCopy() }
