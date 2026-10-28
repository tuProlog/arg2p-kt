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
