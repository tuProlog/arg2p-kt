"""Prolog terms as Python values: atoms as strings, numbers as numbers, lists as lists, the others as [Struct] and
[Var]."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Callable, Union


@dataclass(frozen=True)
class Var:
    """A variable, by its name."""

    name: str

    def __str__(self) -> str:
        return self.name


@dataclass(frozen=True)
class Struct:
    """A compound term: its functor and arguments, and its text, as Prolog writes it."""

    functor: str
    args: tuple[Any, ...]
    text: str = field(default="", compare=False, repr=False)

    @property
    def arity(self) -> int:
        return len(self.args)

    def __str__(self) -> str:
        return self.text or f"{self.functor}({', '.join(map(str, self.args))})"


Value = Union[str, int, float, list, Struct, Var]


def to_python(term: Any, text: Callable[[Any], str]) -> Value:
    """The Python value of [term], a term of 2P-Kt; [text] writes a compound term as Prolog does."""
    if term.isVar():
        return Var(str(term.getName()))
    if term.isList():
        return [to_python(item, text) for item in term.castToList().toList()]
    if term.isAtom():
        return str(term.getValue())
    if term.isInteger():
        return int(str(term.getIntValue().toString()))
    if term.isReal():
        return float(str(term.getDecimalValue().toString()))
    if term.isStruct():
        return Struct(
            str(term.getFunctor()),
            tuple(to_python(argument, text) for argument in term.getArgs()),
            text(term),
        )
    return text(term)
