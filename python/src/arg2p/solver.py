"""Evaluating a theory of arg2p: its solver, answering queries and Prolog goals, and the graphs of its labellings."""

from __future__ import annotations

import re
from dataclasses import dataclass, field
from typing import Any, Optional

from . import _jvm
from .flags import Flags
from .model import Graph, graph_of
from .terms import Value, to_python

_CORE = "it.unibo.tuprolog.argumentation.core"

# The negations written as arg2p writes them: -a and ~a, rather than - a and ~ a
_PREFIX = re.compile(r"(^|[(\[,|]\s*)([-~]) (?=\S)")


class Arg2pError(Exception):
    """An evaluation halted by an error: a theory the profile refuses, a goal that cannot be parsed, ..."""


@dataclass(frozen=True)
class Answer:
    """The statements for a query labelled in, out and und (in_, as in is a keyword of Python)."""

    in_: list[Value] = field(default_factory=list)
    out: list[Value] = field(default_factory=list)
    und: list[Value] = field(default_factory=list)


class Solver:
    """
    The solver of a theory of arg2p, with [flags] (see Flags): queries are answered (answer), Prolog goals solved
    (solve), the graph of each labelling read (graphs). The theory is Prolog text in the language of arg2p.
    """

    def __init__(self, theory: str = "", flags: Optional[Flags] = None) -> None:
        settings = (flags or Flags()).builder().create()
        factory = _jvm.jclass(f"{_CORE}.Arg2pSolverFactory").INSTANCE
        empty = _jvm.jclass("java.util.Collections").emptyList()
        self._solver = _jvm.run(lambda: factory.default(theory, settings, empty, empty))
        operators = self._solver.getOperators()
        self._parser = _jvm.companion("it.unibo.tuprolog.core.parsing.TermParser").withOperators(
            operators, _jvm.companion("it.unibo.tuprolog.core.Scope").empty()
        )
        # the terms written with the operators of arg2p: -a rather than '-'(a)
        self._formatter = _jvm.companion("it.unibo.tuprolog.core.TermFormatter").prettyExpressions(
            operators, _jvm.jclass("it.unibo.tuprolog.core.TermFormatter$TagsFormattingOptions")()
        )
        self._annotations = _jvm.jclass(f"{_CORE}.runtime.theory.RuleAnnotations").INSTANCE

    def _text(self, term: Any) -> str:
        return _PREFIX.sub(r"\1\2", str(self._formatter.format(term)))

    # a statement written by arg2p, written again with its operators
    def _statement(self, text: str) -> str:
        try:
            return self._text(self._parser.parseTerm(text))
        except Exception:  # noqa: BLE001 - kept as it is
            return text

    def _value(self, term: Any) -> Value:
        return to_python(term, self._text)

    def _goal(self, goal: str) -> Any:
        try:
            return self._parser.parseStruct(goal.strip().removesuffix("."))
        except Exception as error:  # a ParseException of 2P-Kt
            raise Arg2pError(f"Not a goal: {goal} ({error})") from error

    def add(self, theory: str) -> None:
        """Adds the clauses of [theory], Prolog text, to the theory."""
        parsed = self._annotations.parse(theory, self._solver.getOperators())
        _jvm.run(lambda: self._solver.appendStaticKb(parsed))

    def solve(self, goal: str, limit: Optional[int] = None) -> list[dict[str, Value]]:
        """The bindings of the variables of [goal], Prolog text, in each of its solutions (at most [limit])."""
        parsed = self._goal(goal)

        def solutions() -> list[dict[str, Value]]:
            found = _jvm.items(self._solver.solve(parsed), limit)
            _raise_halt(found)
            return [
                {str(entry.getKey().getName()): self._value(entry.getValue()) for entry in solution.getSubstitution().entrySet()}
                for solution in found
                if solution.isYes()
            ]

        return _jvm.run(solutions)

    def answer(self, query: str) -> Answer:
        """The statements for [query], Prolog text, labelled in, out and und (see answerQuery/4)."""
        query = query.strip().removesuffix(".")
        found = self.solve(f"answerQuery(({query}), Arg2pIn, Arg2pOut, Arg2pUnd)", limit=1)
        if not found:
            return Answer()
        return Answer(found[0]["Arg2pIn"], found[0]["Arg2pOut"], found[0]["Arg2pUnd"])

    def graphs(self) -> list[Graph]:
        """The graph of each labelling of the theory (several for a semantics with several, as preferred)."""
        mining = _jvm.jclass(f"{_CORE}.model.mining.GraphMiningKt")
        goal = self._goal("buildLabelSetsSilent")

        def graphs() -> list[Graph]:
            found: list[Graph] = []

            # the graph of a labelling read while the solver is at its solution
            def read(solution: Any) -> Any:
                if solution.isYes():
                    found.append(graph_of(mining.graph(self._solver), self._statement))
                return solution

            _raise_halt(_jvm.items(self._solver.solve(goal), each=read))
            return found

        return _jvm.run(graphs)


def _raise_halt(solutions: list[Any]) -> None:
    for solution in solutions:
        if solution.isHalt():
            error = solution.getException()
            raise Arg2pError(str(error.getMessage() or error.toString()) if error is not None else str(solution))


def evaluate(theory: str, flags: Optional[Flags] = None) -> list[Graph]:
    """The graph of each labelling of [theory] (see Solver.graphs)."""
    return Solver(theory, flags).graphs()


def answer_query(theory: str, query: str, flags: Optional[Flags] = None) -> Answer:
    """The statements for [query] in [theory] labelled in, out and und (see Solver.answer)."""
    return Solver(theory, flags).answer(query)
