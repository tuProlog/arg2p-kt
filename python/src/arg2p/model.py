"""The argumentation graph of a labelling, as plain data: its arguments, their labels, attacks and supports."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Optional


@dataclass(frozen=True)
class Argument:
    """
    An argument: its rules, its top rule (none for a premise) and its conclusion, with the arguments it is built on
    (supports), identified as A0, A1, ... in the graph (identifier); descriptor writes it as A2 : A0,r1 : c.
    """

    identifier: str
    descriptor: str
    conclusion: str
    top_rule: str
    rules: tuple[str, ...] = ()
    supports: tuple[Argument, ...] = field(default=(), repr=False)


@dataclass(frozen=True)
class Attack:
    """[attacker] attacks [target], with its type (rebut, undermine, undercut, ...) and the sub-argument it hits."""

    attacker: Argument
    target: Argument
    type: Optional[str] = None
    on: Optional[Argument] = None


@dataclass(frozen=True)
class Support:
    """[supporter] is an argument [supported] is built on."""

    supporter: Argument
    supported: Argument


@dataclass(frozen=True)
class LabelledArgument:
    """An argument with its label: in, out, und (na for an argument the labelling does not label)."""

    argument: Argument
    label: str


@dataclass(frozen=True)
class Graph:
    """An argumentation graph: its arguments, the attacks and supports between them, and the label of each argument."""

    arguments: tuple[Argument, ...]
    attacks: tuple[Attack, ...] = ()
    supports: tuple[Support, ...] = ()
    labellings: tuple[LabelledArgument, ...] = ()

    def label(self, argument: Argument) -> Optional[str]:
        """The label of [argument]: in, out, und, or na for an argument the labelling does not label."""
        return next((labelled.label for labelled in self.labellings if labelled.argument == argument), None)

    def labelled(self, label: str) -> list[Argument]:
        """The arguments labelled [label] (in, out, und)."""
        return [labelled.argument for labelled in self.labellings if labelled.label == label]

    def conclusions(self, label: str) -> list[str]:
        """The conclusions of the arguments labelled [label] (in, out, und), each once."""
        return list(dict.fromkeys(argument.conclusion for argument in self.labelled(label)))


def graph_from_jvm(graph: Any) -> Graph:
    """The graph of [graph], a Graph of the library of the JVM."""
    arguments: dict[str, Argument] = {}

    def argument(of: Any) -> Argument:
        identifier = str(of.getIdentifier())
        if identifier not in arguments:
            arguments[identifier] = Argument(
                identifier,
                str(of.getDescriptor()),
                str(of.getConclusion()),
                str(of.getTopRule()),
                tuple(str(rule) for rule in of.getRules()),
                tuple(argument(support) for support in of.getSupports()),
            )
        return arguments[identifier]

    return Graph(
        tuple(argument(of) for of in graph.getArguments()),
        tuple(
            Attack(
                argument(of.getAttacker()),
                argument(of.getTarget()),
                None if of.getType() is None else str(of.getType()),
                None if of.getOn() is None else argument(of.getOn()),
            )
            for of in graph.getAttacks()
        ),
        tuple(Support(argument(of.getSupporter()), argument(of.getSupported())) for of in graph.getSupports()),
        tuple(LabelledArgument(argument(of.getArgument()), str(of.getLabel())) for of in graph.getLabellings()),
    )


class CompactGraph:
    """
    All that a Graph tells, as arrays, with no object for an argument: what a graph too large for Graph is read as. It holds no
    reference to the solver. An argument is a position, from 0 to size, in every array; a text is a position in texts.

    conclusions and top_rules give a text for each argument; rules, groundings, defeasible_rules, defeasible_premises and
    last_defeasible_rules a list of texts (see of); labels a position in label_names. attackers, targets, on and attack_type_of are
    parallel, as supporters and supported: positions of arguments. The arrays are those of the JVM, not copied: indexed, iterated,
    or read at once with numpy.asarray.
    """

    def __init__(self, graph: Any) -> None:
        self._graph = graph

    size = property(lambda self: int(self._graph.getSize()))
    conclusions = property(lambda self: self._graph.getConclusions())
    top_rules = property(lambda self: self._graph.getTopRules())
    labels = property(lambda self: self._graph.getLabels())
    attackers = property(lambda self: self._graph.getAttackers())
    targets = property(lambda self: self._graph.getTargets())
    on = property(lambda self: self._graph.getOn())
    attack_type_of = property(lambda self: self._graph.getAttackTypeOf())
    supporters = property(lambda self: self._graph.getSupporters())
    supported = property(lambda self: self._graph.getSupported())
    texts = property(lambda self: self._graph.getTexts())
    order = property(lambda self: self._graph.getOrder())

    @property
    def attack_types(self) -> list[str]:
        """The types of the attacks, as attack_type_of tells them."""
        return [str(it) for it in self._graph.getAttackTypes()]

    @property
    def label_names(self) -> list[str]:
        """What labels tells."""
        return [str(it) for it in self._graph.getLabelNames()]

    def conclusion(self, position: int) -> str:
        """The conclusion of the argument at [position]."""
        return str(self.texts[self.conclusions[position]])

    def top_rule(self, position: int) -> str:
        """The top rule of the argument at [position]."""
        return str(self.texts[self.top_rules[position]])

    def rules(self, position: int) -> list[str]:
        """The rules of the argument at [position]."""
        return [str(self.texts[it]) for it in self._graph.getRules().of(position)]

    def groundings(self, position: int) -> list[str]:
        """The body of the argument at [position]."""
        return [str(self.texts[it]) for it in self._graph.getGroundings().of(position)]

    def defeasible_rules(self, position: int) -> list[str]:
        """The defeasible rules of the argument at [position]."""
        return [str(self.texts[it]) for it in self._graph.getDefeasibleRules().of(position)]

    def defeasible_premises(self, position: int) -> list[str]:
        """The defeasible premises of the argument at [position]."""
        return [str(self.texts[it]) for it in self._graph.getDefeasiblePremises().of(position)]

    def last_defeasible_rules(self, position: int) -> list[str]:
        """The last defeasible rules of the argument at [position]."""
        return [str(self.texts[it]) for it in self._graph.getLastDefeasibleRules().of(position)]

    def label(self, position: int) -> str:
        """The label of the argument at [position]: in, out, und, or na."""
        return str(self._graph.label(position))

    def labelled(self, label: str) -> Any:
        """The arguments labelled [label] (in, out, und)."""
        return self._graph.labelled(label)

    def identifier(self, position: int) -> str:
        """The name of the argument at [position], as Graph names it: A0, A1, ..."""
        return str(self._graph.identifier(position))

    def descriptor(self, position: int) -> str:
        """The argument at [position] written as A2 : A0,r1 : c."""
        return str(self._graph.descriptor(position))
