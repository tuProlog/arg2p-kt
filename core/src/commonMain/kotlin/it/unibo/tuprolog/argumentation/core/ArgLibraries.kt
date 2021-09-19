package it.unibo.tuprolog.argumentation.core

import it.unibo.tuprolog.argumentation.core.libs.RuleParser
import it.unibo.tuprolog.argumentation.core.libs.extra.MetaInterpreter
import it.unibo.tuprolog.argumentation.core.libs.extra.ModuleCalls
import it.unibo.tuprolog.argumentation.core.libs.graph.AbstractMode
import it.unibo.tuprolog.argumentation.core.libs.graph.builder.ArgumentationGraphBuilder
import it.unibo.tuprolog.argumentation.core.libs.graph.extension.AttackRestrictionHandler
import it.unibo.tuprolog.argumentation.core.libs.graph.extension.BpMetaGraphHandler
import it.unibo.tuprolog.argumentation.core.libs.graph.extension.DefeasiblePreferencesHandler
import it.unibo.tuprolog.argumentation.core.libs.graph.extension.GenericDefeasiblePreferencesHandler
import it.unibo.tuprolog.argumentation.core.libs.graph.extension.StrictPreferencesHandler
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.BpLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.CompleteLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.GroundedLabeller
import it.unibo.tuprolog.argumentation.core.libs.graph.labelling.StatementLabeller
import it.unibo.tuprolog.argumentation.core.libs.structured.StructuredMode
import it.unibo.tuprolog.solve.library.AliasedLibrary

enum class ArgLibraries {

    META {
        override fun identifier() = "interpreter"
        override fun library() = MetaInterpreter
    },
    MODULE {
        override fun identifier() = "module"
        override fun library() = ModuleCalls
    },
    ABSTRACT {
        override fun identifier() = "abstract"
        override fun library() = AbstractMode
    },
    GRAPH {
        override fun identifier() = "graph"
        override fun library() = ArgumentationGraphBuilder
    },
    REBUT {
        override fun identifier() = "rebut"
        override fun library() = AttackRestrictionHandler
    },
    METABP {
        override fun identifier() = "metabp"
        override fun library() = BpMetaGraphHandler
    },
    DEFPREF {
        override fun identifier() = "defpref"
        override fun library() = DefeasiblePreferencesHandler
    },
    GDEFPREF {
        override fun identifier() = "gdefpref"
        override fun library() = GenericDefeasiblePreferencesHandler
    },
    STRICTPREF {
        override fun identifier() = "strictpref"
        override fun library() = StrictPreferencesHandler
    },
    BP {
        override fun identifier() = "bp"
        override fun library() = BpLabeller
    },
    COMPLETE {
        override fun identifier() = "complete"
        override fun library() = CompleteLabeller
    },
    GROUNDED {
        override fun identifier() = "grounded"
        override fun library() = GroundedLabeller
    },
    STATEMENT {
        override fun identifier() = "statement"
        override fun library() = StatementLabeller
    },
    STRUCTURED {
        override fun identifier() = "structured"
        override fun library() = StructuredMode
    },
    PARSER {
        override fun identifier() = "parser"
        override fun library() = RuleParser
    };

    abstract fun identifier(): String
    abstract fun library(): AliasedLibrary

    companion object {
        fun fromIdentifier(identifier: String) = values().first { it.identifier() == identifier }.library()
    }
}
