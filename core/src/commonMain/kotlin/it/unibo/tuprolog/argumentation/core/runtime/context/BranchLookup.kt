package it.unibo.tuprolog.argumentation.core.runtime.context

import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Struct

// The lookup of a goal in a branch: context_check(Branch, Goal)
internal fun branchLookup(branch: Int) = StoreLookup { Struct.of("context_check", Integer.of(branch), it) }
