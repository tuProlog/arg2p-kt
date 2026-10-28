"""The flags of a solver (see the flags reference of arg2p), and the values they take."""

from __future__ import annotations

from enum import Enum
from typing import Any

from . import _jvm

_API = "it.unibo.tuprolog.argumentation"


class _FlagValue(Enum):
    """A value of a flag: [value] is the atom stating it in a theory."""

    def jvm(self) -> Any:
        """The value of the library of the JVM."""
        return getattr(_jvm.jclass(f"{_API}.{type(self).__name__}"), self.name)


class Profile(_FlagValue):
    """The profile of a theory (graphBuildMode): the flags its definition needs, preset."""

    FREE = "free"
    ASPIC_PLUS = "aspic_plus"
    ASPIC = "aspic"
    ASPIC_DEFEASIBLE = "aspic_defeasible"
    META = "meta"
    ABA = "aba"
    ABA_PLUS = "aba_plus"
    ABSTRACT = "abstract"


class Semantics(_FlagValue):
    """The semantics of the argument labelling (argumentLabellingMode)."""

    CONFLICT_FREE = "conflictfree"
    ADMISSIBLE = "admissible"
    STRONGLY_ADMISSIBLE = "stronglyadmissible"
    GROUNDED = "grounded"
    COMPLETE = "complete"
    PREFERRED = "preferred"
    SEMI_STABLE = "semistable"
    STABLE = "stable"
    IDEAL = "ideal"
    EAGER = "eager"
    NAIVE = "naive"
    STAGE = "stage"
    CF2 = "cf2"
    STAGE2 = "stage2"
    BP_GROUNDED = "bp_grounded"
    BP_GROUNDED_PARTIAL = "bp_grounded_partial"
    BP_GROUNDED_COMPLETE = "bp_grounded_complete"


class StatementLabelling(_FlagValue):
    """The labelling of the statements from that of the arguments (statementLabellingMode)."""

    STATEMENT = "statement"
    STATEMENT_PASS_THROUGH = "statement_pass_through"
    STATEMENT_BINARY = "statement_binary"


class OrderingPrinciple(_FlagValue):
    """The ordering principle of the preferences (orderingPrinciple)."""

    LAST = "last"
    WEAKEST = "weakest"


class OrderingComparator(_FlagValue):
    """The comparator of the sets of rules or premises (orderingComparator)."""

    ELITIST = "elitist"
    DEMOCRAT = "democrat"
    NORMAL = "normal"


class GraphExtension(_FlagValue):
    """An extension of the graph (graphExtension)."""

    REBUT_RESTRICTION = "rebutRestriction"
    BP = "bp"
    STANDARD_PREF = "standardPref"
    DEFEASIBLE_PREF = "defeasiblePref"
    DEFEASIBLE_ALL_PREF = "defeasibleAllPref"
    ABA_PREF = "abaPref"


class GraphLanguage(_FlagValue):
    """A language extending the one of arg2p (graphLanguage)."""

    ASPIC_LANGUAGE = "aspicLanguage"
    ABA_LANGUAGE = "abaLanguage"
    DEFEASIBLE_LANGUAGE = "defeasibleLanguage"


class GraphBuilder(_FlagValue):
    """The module building the graph (graphBuilder): from the rules, or from the arguments and attacks."""

    RULE_GRAPH = "ruleGraph"
    ABSTRACT_GRAPH = "abstractGraph"


class ConflictModel(_FlagValue):
    """The conflicts beyond those a theory declares (conflictModel)."""

    BUILTIN = "builtin"
    DECLARED = "declared"


def _array(cls: type, values: tuple) -> Any:
    return _jvm.jarray(f"{_API}.{cls.__name__}", [value.jvm() for value in values])


class Flags:
    """
    The flags of a solver, as the facts stating them: defaults() (those of arg2p), none() (only those the theory
    states), or parse(text). A flag set replaces what the flags stated for it; the theory may state flags as well,
    which override these. The profile of the theory (graph_build_mode) presets the flags its definition needs.
    """

    def __init__(self, flags: Any) -> None:
        self._flags = flags

    @staticmethod
    def defaults() -> Flags:
        """The flags of arg2p by default."""
        return Flags(_jvm.jclass(f"{_API}.Flags").defaults())

    @staticmethod
    def none() -> Flags:
        """No flags: the solver has only those the theory states, the others are the defaults of their modules."""
        return Flags(_jvm.jclass(f"{_API}.Flags").none())

    @staticmethod
    def parse(text: str) -> Flags:
        """The flags stated by the facts of [text], Prolog text (argumentLabellingMode(preferred). queryMode.)."""
        return Flags(_jvm.jclass(f"{_API}.Flags").parse(text))

    def jvm(self) -> Any:
        """The flags of the library of the JVM."""
        return self._flags

    def graph_build_mode(self, profile: Profile) -> Flags:
        """The profile of the theory."""
        return self._set(self._flags.graphBuildMode(profile.jvm()))

    def argument_labelling_mode(self, semantics: Semantics) -> Flags:
        """The semantics of the argument labelling."""
        return self._set(self._flags.argumentLabellingMode(semantics.jvm()))

    def statement_labelling_mode(self, labelling: StatementLabelling) -> Flags:
        """The labelling of the statements."""
        return self._set(self._flags.statementLabellingMode(labelling.jvm()))

    def ordering_principle(self, principle: OrderingPrinciple) -> Flags:
        """The ordering principle of the preferences."""
        return self._set(self._flags.orderingPrinciple(principle.jvm()))

    def ordering_comparator(self, comparator: OrderingComparator) -> Flags:
        """The comparator of the preferences."""
        return self._set(self._flags.orderingComparator(comparator.jvm()))

    def graph_extensions(self, *extensions: GraphExtension) -> Flags:
        """The extensions of the graph: none for no extension."""
        return self._set(self._flags.graphExtensions(_array(GraphExtension, extensions)))

    def graph_language(self, *languages: GraphLanguage) -> Flags:
        """The languages extending the one of arg2p: none for no language."""
        return self._set(self._flags.graphLanguage(_array(GraphLanguage, languages)))

    def graph_builder(self, builder: GraphBuilder) -> Flags:
        """The module building the graph."""
        return self._set(self._flags.graphBuilder(builder.jvm()))

    def conflict_model(self, model: ConflictModel) -> Flags:
        """The conflicts beyond those the theory declares."""
        return self._set(self._flags.conflictModel(model.jvm()))

    def modules_path(self, path: str) -> Flags:
        """Where the modules loaded by name are looked for."""
        return self._set(self._flags.modulesPath(path))

    def query_mode(self, on: bool) -> Flags:
        """Whether the graph is built on the arguments the query depends on alone."""
        return self._set(self._flags.queryMode(on))

    def auto_transposition(self, on: bool) -> Flags:
        """Whether the strict rules are transposed."""
        return self._set(self._flags.autoTransposition(on))

    def prolog_strict_compatibility(self, on: bool) -> Flags:
        """Whether the theory is read as strict Prolog (no rules of arg2p)."""
        return self._set(self._flags.prologStrictCompatibility(on))

    def meta_conflicts(self, on: bool) -> Flags:
        """Whether the conflicts are arguments as well."""
        return self._set(self._flags.metaConflicts(on))

    def meta_rules(self, on: bool) -> Flags:
        """Whether the rules are arguments as well."""
        return self._set(self._flags.metaRules(on))

    def debug(self, on: bool) -> Flags:
        """Whether the modules print their debug output."""
        return self._set(self._flags.debug(on))

    def _set(self, flags: Any) -> Flags:
        self._flags = flags
        return self

    def __str__(self) -> str:
        """The flags as the facts stating them, Prolog text."""
        return str(self._flags.toString())

    def __repr__(self) -> str:
        return f"Flags({str(self)!r})"

    def __eq__(self, other: object) -> bool:
        return isinstance(other, Flags) and bool(self._flags.equals(other._flags))

    def __hash__(self) -> int:
        return int(self._flags.hashCode())
