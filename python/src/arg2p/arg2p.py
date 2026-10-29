"""A solver of arg2p: its answers, graphs and solutions, and the causality tests on its theory."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Callable, Iterator, Optional

import jpype

from . import _jvm
from .flags import Flags
from .model import Graph, graph_from_jvm
from .values import Value, from_jvm

_API = "it.unibo.tuprolog.argumentation"


@dataclass(frozen=True)
class SyntaxProblem:
    """
    Where a text (a theory, a query, flags) does not parse: its line and column (from 1), the token found there when
    there is one, and why (as "Unexpected token '.'; expected term").
    """

    line: int
    column: int
    token: Optional[str]
    description: str

    @property
    def length(self) -> int:
        """How many characters the problem spans from its column: the token, or one."""
        return len(self.token) if self.token else 1

    def __str__(self) -> str:
        return f"{self.description} at {self.line}:{self.column}"


def problem_from_jvm(problem: Any) -> Optional[SyntaxProblem]:
    """A SyntaxProblem of the library of the JVM, or None."""
    if problem is None:
        return None
    token = problem.getToken()
    return SyntaxProblem(
        line=int(problem.getLine()),
        column=int(problem.getColumn()),
        token=None if token is None else str(token),
        description=str(problem.getDescription()),
    )


class Arg2pError(Exception):
    """
    An evaluation halted by an error: a theory its profile refuses, a goal that cannot be parsed, ... When a text does
    not parse, syntax tells where.
    """

    def __init__(self, message: str, syntax: Optional[SyntaxProblem] = None) -> None:
        super().__init__(message)
        self.syntax = syntax


def call(function: Callable[[], Any]) -> Any:
    """The result of [function], run on a thread of the JVM, an exception of arg2p raised as an Arg2pError."""

    def guarded() -> Any:
        try:
            return function()
        except jpype.JException as error:
            if error.getClass().getName() == f"{_API}.Arg2pException":
                raise Arg2pError(str(error.getMessage()), problem_from_jvm(error.getSyntax())) from None
            raise

    return _jvm.run(guarded)


@dataclass(frozen=True)
class Answer:
    """The statements for a query labelled in, out and und (see Arg2p.answer)."""

    in_statements: list[Value] = field(default_factory=list)
    out_statements: list[Value] = field(default_factory=list)
    und_statements: list[Value] = field(default_factory=list)
    # the answer of the library of the JVM, for label
    _answer: Any = field(default=None, compare=False, repr=False)

    def label(self, statement: str) -> Optional[str]:
        """The label of [statement], Prolog text: in, out or und, None for a statement the query did not label."""
        if self._answer is None:
            raise ValueError("Not the answer of a solver")
        label = call(lambda: self._answer.label(statement))
        return None if label is None else str(label)


def answer_from_jvm(answer: Any) -> Answer:
    """The answer of [answer], an Answer of the library of the JVM."""
    return Answer(
        [from_jvm(it) for it in answer.getInStatements()],
        [from_jvm(it) for it in answer.getOutStatements()],
        [from_jvm(it) for it in answer.getUndStatements()],
        answer,
    )


class Solution:
    """A solution of a goal: yes, no or halt (an error), with the values a yes solution binds to its variables."""

    def __init__(self, solution: Any) -> None:
        self._solution = solution

    @property
    def is_yes(self) -> bool:
        return bool(self._solution.isYes())

    @property
    def is_no(self) -> bool:
        return bool(self._solution.isNo())

    @property
    def is_halt(self) -> bool:
        return bool(self._solution.isHalt())

    @property
    def query(self) -> str:
        """The goal, as a yes solution solves it (its variables bound), otherwise as it was asked, as Prolog text."""
        return str(self._solution.getQuery())

    @property
    def error(self) -> Optional[str]:
        """The error of a halt solution."""
        error = self._solution.getError()
        return None if error is None else str(error)

    @property
    def variables(self) -> list[str]:
        """The variables of the goal a yes solution binds."""
        return [str(name) for name in self._solution.getVariables()]

    @property
    def bindings(self) -> dict[str, Value]:
        """The values a yes solution binds to the variables of the goal, by their names."""
        return {str(entry.getKey()): from_jvm(entry.getValue()) for entry in self._solution.getBindings().entrySet()}

    def value(self, name: str) -> Optional[Value]:
        """The value a yes solution binds to the variable of the goal named [name]."""
        value = self._solution.value(name)
        return None if value is None else from_jvm(value)

    def text(self, name: str) -> Optional[str]:
        """The value a yes solution binds to the variable of the goal named [name], as Prolog text."""
        written = self._solution.text(name)
        return None if written is None else str(written)

    def __repr__(self) -> str:
        return str(self._solution.toString())


class Solutions:
    """
    The solutions of a goal, computed one at a time as they are asked for: with has_next and next, or iterated. An
    exception of arg2p computing one is raised as an Arg2pError.
    """

    def __init__(self, solutions: Any) -> None:
        self._solutions = solutions

    def has_next(self) -> bool:
        return bool(call(self._solutions.hasNext))

    def next(self) -> Solution:
        return Solution(call(self._solutions.next))

    def to_list(self, limit: Optional[int] = None) -> list[Solution]:
        """The solutions left, at most [limit] of them."""
        return [Solution(it) for it in call(lambda: self._solutions.toList(2**31 - 1 if limit is None else limit))]

    def __iter__(self) -> Iterator[Solution]:
        return self

    def __next__(self) -> Solution:
        # the next solution, if there is one, in a single call
        found = call(lambda: self._solutions.next() if self._solutions.hasNext() else None)
        if found is None:
            raise StopIteration
        return Solution(found)


class Causality:
    """
    Whether a statement of the theory of a solver causes another, on the grounded labelling of the theory and of its
    interventions: the NESS test, as published (Pisano, Prakken, Sartor, Liepina; ICAIL 2025) and revised, and the
    but-for test. The statements are Prolog text; an intervention is a list of statements.
    """

    def __init__(self, causality: Any) -> None:
        self._causality = causality

    def ness(self, cause: str, effect: str) -> Optional[Value]:
        """Whether [cause] is a NESS cause of [effect] (the revised condition): the intervention, None if it is not."""
        return _optional(call(lambda: self._causality.ness(cause, effect)))

    def ness_original(self, cause: str, effect: str) -> Optional[Value]:
        """Whether [cause] is a NESS cause of [effect] as published: the intervention, None if it is not."""
        return _optional(call(lambda: self._causality.nessOriginal(cause, effect)))

    def ness_intervention(self, intervention: str, effect: str) -> bool:
        """Whether [intervention], a list of statements, meets the revised NESS condition for [effect]."""
        return bool(call(lambda: self._causality.nessIntervention(intervention, effect)))

    def ness_original_intervention(self, intervention: str, effect: str) -> bool:
        """Whether [intervention], a list of statements, meets the NESS condition for [effect] as published."""
        return bool(call(lambda: self._causality.nessOriginalIntervention(intervention, effect)))

    def but_for(self, cause: str, effect: str) -> bool:
        """Whether [cause] is a but-for cause of [effect]."""
        return bool(call(lambda: self._causality.butFor(cause, effect)))


def _optional(value: Any) -> Optional[Value]:
    return None if value is None else from_jvm(value)


class Arg2p:
    """
    A solver of arg2p for a theory, as Prolog text, with its flags: it answers queries (answer), gives the graph of
    each labelling (graphs), solves Prolog goals (solve) and tests causality (causality).

        >>> Arg2p.of("r1 : [] => a.\\nr2 : [] => -a.\\nsup(r1, r2).").answer("a").in_statements
        ['a']

    The context of the solver keeps a branch for every stage of a resolution, numbered from 0: active_branch is the
    last one, graph_at the graph of any of them, and context_facts what a branch stores.
    """

    def __init__(self, arg2p: Any) -> None:
        self._arg2p = arg2p
        self.causality = Causality(arg2p.getCausality())

    @staticmethod
    def of(theory: str = "", flags: Optional[Flags] = None) -> Arg2p:
        """A solver of arg2p for [theory], Prolog text, with [flags] (Flags.defaults() otherwise)."""
        return Arg2p.builder().theory(theory).flags(flags or Flags.defaults()).build()

    @staticmethod
    def builder() -> Builder:
        """A solver of arg2p, as it is described."""
        return Builder()

    @staticmethod
    def check_theory(theory: str) -> Optional[SyntaxProblem]:
        """Where [theory], Prolog text (a theory, or flags), does not parse; None if it does. Nothing is solved."""
        return call(lambda: problem_from_jvm(_jvm.jclass(f"{_API}.Arg2p").checkTheory(theory)))

    @staticmethod
    def check_query(query: str) -> Optional[SyntaxProblem]:
        """Where [query], a goal in Prolog text, does not parse; None if it does. Nothing is solved."""
        return call(lambda: problem_from_jvm(_jvm.jclass(f"{_API}.Arg2p").checkQuery(query)))

    @staticmethod
    def version() -> str:
        """The version of arg2p, the released one for a release."""
        return str(_jvm.jclass(f"{_API}.Arg2p").version())

    @property
    def theory(self) -> str:
        """The theory, as Prolog text."""
        return str(self._arg2p.getTheory())

    @property
    def known_flags(self) -> list[str]:
        """The flags the solver knows, by name: graphBuildMode, queryMode, ... (see setting)."""
        return [str(flag) for flag in self._arg2p.getKnownFlags()]

    def setting(self, flag: str) -> list[str]:
        """
        The values of [flag] in effect for the theory, as the solver checks them: those the theory states, or else
        those of the flags of the solver that differ from their default, or else those the profile of the theory
        presets. A switch on has the value "true", off none.
        """
        return [str(value) for value in call(lambda: self._arg2p.setting(flag))]

    def answer(self, query: str) -> Answer:
        """The statements for [query], Prolog text, labelled in, out and und (raises Arg2pError if it halts)."""
        return answer_from_jvm(call(lambda: self._arg2p.answer(query)))

    def graphs(self) -> list[Graph]:
        """The graph of each labelling of the theory: several for a semantics with several (preferred, stable, ...)."""
        return call(lambda: [graph_from_jvm(graph) for graph in self._arg2p.graphs()])

    def graph(self) -> Graph:
        """The graph of the last resolution: the one of its last stage."""
        return call(lambda: graph_from_jvm(self._arg2p.graph()))

    def graph_at(self, branch: int) -> Graph:
        """The graph of the context branch [branch]."""
        return call(lambda: graph_from_jvm(self._arg2p.graphAt(branch)))

    @property
    def active_branch(self) -> int:
        """The context branch of the last stage of the last resolution."""
        return int(call(self._arg2p.getActiveBranch))

    def solve(self, goal: str) -> Solutions:
        """The solutions of [goal], Prolog text, computed one at a time as they are asked for."""
        return Solutions(call(lambda: self._arg2p.solve(goal)))

    def context_facts(self, branch: int, goal: str) -> Solutions:
        """What the context branch [branch] stores matching [goal], Prolog text: the solutions of context_check/2."""
        return Solutions(call(lambda: self._arg2p.contextFacts(branch, goal)))

    def add(self, theory: str) -> None:
        """Adds the clauses of [theory], Prolog text, to the theory."""
        call(lambda: self._arg2p.add(theory))

    def load(self, theory: str) -> None:
        """Replaces the theory with [theory], Prolog text."""
        call(lambda: self._arg2p.load(theory))


class Builder:
    """A solver of arg2p, as it is described: its theory, its flags, and where what it prints goes."""

    def __init__(self) -> None:
        self._builder = _jvm.jclass(f"{_API}.Arg2p").builder()

    def theory(self, theory: str) -> Builder:
        """The theory, as Prolog text."""
        self._builder.theory(theory)
        return self

    def flags(self, flags: Flags) -> Builder:
        """The flags (Flags.defaults() otherwise)."""
        self._builder.flags(flags.jvm())
        return self

    def output(self, consumer: Callable[[str], Any]) -> Builder:
        """The function receiving what the solver prints (the standard output otherwise)."""
        self._builder.output(_jvm.function1(lambda printed: consumer(str(printed))))
        return self

    def build(self) -> Arg2p:
        return Arg2p(call(self._builder.build))
