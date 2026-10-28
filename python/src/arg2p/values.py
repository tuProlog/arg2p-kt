"""The values arg2p gives and takes, as Python values: atoms as strings, numbers as numbers, lists as lists, compound
terms as [Struct] and variables as [Var] (the Value of the library of the JVM)."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Union


@dataclass(frozen=True)
class Var:
    """A variable, by its name."""

    name: str

    def __str__(self) -> str:
        return self.name


@dataclass(frozen=True)
class Struct:
    """A compound term: its functor and arguments, and its text, as arg2p writes it (-a, o(-b))."""

    functor: str
    args: tuple[Any, ...]
    text: str = field(default="", compare=False, repr=False)

    @property
    def arity(self) -> int:
        return len(self.args)

    def __str__(self) -> str:
        return self.text or f"{self.functor}({', '.join(map(str, self.args))})"


Value = Union[str, int, float, list, Struct, Var]


def from_jvm(value: Any) -> Value:
    """The Python value of [value], a Value of the library of the JVM."""
    kind = str(value.getKind())
    if kind == "atom":
        return str(value.getName())
    if kind == "integer":
        return int(value.getValue())
    if kind == "real":
        return float(value.getValue())
    if kind == "variable":
        return Var(str(value.getName()))
    if kind == "list":
        return [from_jvm(item) for item in value.getItems()]
    return Struct(str(value.getFunctor()), tuple(from_jvm(argument) for argument in value.getArgs()), str(value.toString()))


def text(value: Value) -> str:
    """[value] as Prolog text, as arg2p writes it."""
    if isinstance(value, list):
        return "[" + ", ".join(text(item) for item in value) + "]"
    return str(value)
