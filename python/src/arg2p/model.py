"""The argumentation graph of an evaluation, as plain data: its arguments, their labels, attacks and supports."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Callable, Optional


@dataclass(frozen=True)
class Argument:
    """
    An argument: its identifier (A0, A1, ... in a stable order), its rules, its top rule (none for a premise), its
    conclusion, the identifiers of its sub-arguments, and a readable descriptor.
    """

    identifier: str
    rules: tuple[str, ...]
    top_rule: str
    conclusion: str
    supports: tuple[str, ...] = ()
    descriptor: str = ""


@dataclass(frozen=True)
class Attack:
    """An attack of [attacker] on [target] (identifiers), of a [type], on its sub-argument [on]."""

    attacker: str
    target: str
    type: Optional[str] = None
    on: Optional[str] = None


@dataclass(frozen=True)
class Support:
    """[supporter] is a sub-argument of [supported] (identifiers)."""

    supporter: str
    supported: str


@dataclass(frozen=True)
class Graph:
    """The graph of one labelling: the arguments, their labels (in, out, und) by identifier, attacks and supports."""

    arguments: tuple[Argument, ...]
    labels: dict[str, str] = field(default_factory=dict)
    attacks: tuple[Attack, ...] = ()
    supports: tuple[Support, ...] = ()

    def argument(self, identifier: str) -> Argument:
        """The argument with [identifier]."""
        return next(argument for argument in self.arguments if argument.identifier == identifier)

    def labelled(self, label: str) -> list[Argument]:
        """The arguments labelled [label] (in, out or und)."""
        return [argument for argument in self.arguments if self.labels.get(argument.identifier) == label]

    def conclusions(self, label: str) -> set[str]:
        """The conclusions of the arguments labelled [label]."""
        return {argument.conclusion for argument in self.labelled(label)}


def graph_of(graph: Any, statement: Callable[[str], str] = str) -> Graph:
    """The Graph of [graph], a Graph of arg2p (its model package), the conclusions written by [statement]."""

    def identifier(argument: Any) -> str:
        return str(argument.getIdentifier())

    arguments = tuple(
        Argument(
            identifier(argument),
            tuple(str(rule) for rule in argument.getRules()),
            str(argument.getTopRule()),
            statement(str(argument.getConclusion())),
            tuple(identifier(support) for support in argument.getSupports()),
            str(argument.getDescriptor()),
        )
        for argument in graph.getArguments()
    )
    labels = {identifier(labelled.getArgument()): str(labelled.getLabel()) for labelled in graph.getLabellings()}
    attacks = tuple(
        Attack(
            identifier(attack.getAttacker()),
            identifier(attack.getTarget()),
            None if attack.getType() is None else str(attack.getType()),
            None if attack.getOn() is None else identifier(attack.getOn()),
        )
        for attack in graph.getAttacks()
    )
    supports = tuple(
        Support(identifier(support.getSupporter()), identifier(support.getSupported())) for support in graph.getSupports()
    )
    return Graph(arguments, labels, attacks, supports)
