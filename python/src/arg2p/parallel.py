"""The parallel solver of arg2p: parallel solvers on the threads of the JVM (ThreadedSolver), building the graph of a theory
with solvers of its parts, and what federates them with other parallel solvers (of the JVM, of Node.js, of browsers) over
WebSockets (Federation)."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Callable, Optional

from . import _jvm
from .arg2p import Answer, Solutions, answer_from_jvm, call
from .flags import Flags
from .model import CompactGraph, Graph, graph_from_jvm

_PARALLEL = "it.unibo.tuprolog.argumentation.parallel"
_FEDERATION = "it.unibo.tuprolog.argumentation.federation"


@dataclass(frozen=True)
class Response:
    """
    An argument for the statement evaluated (claim, the statement), with the argument itself in query_chain (empty for
    a statement no argument concludes), as Prolog text.
    """

    claim: str
    query_chain: tuple[str, ...] = ()


@dataclass(frozen=True)
class EvaluationResponse:
    """The statements answering a statement, by label, each with the arguments for it; error, if one kept it from an answer."""

    in_args: list[Response] = field(default_factory=list)
    out_args: list[Response] = field(default_factory=list)
    und_args: list[Response] = field(default_factory=list)
    error: Optional[str] = None


def _response(response: Any) -> EvaluationResponse:
    def responses(of: Any) -> list[Response]:
        return [Response(str(it.getClaim()), tuple(str(argument) for argument in it.getQueryChain())) for it in of]

    error = response.getError()
    return EvaluationResponse(
        responses(response.getInArgs()),
        responses(response.getOutArgs()),
        responses(response.getUndArgs()),
        None if error is None else str(error),
    )


@dataclass(frozen=True)
class SiteStats:
    """A site of a federation: its identifier, its name (empty if it has none), and the consumers it offers (None for no limit)."""

    id: str
    name: str
    workers: Optional[int]


@dataclass(frozen=True)
class WorkerStats:
    """A consumer of a graph: its partition of the theory, the site that ran it, the arguments it built, the attacks it saved."""

    partition: int
    site: str
    arguments: int
    attacks: int


@dataclass(frozen=True)
class EvaluationStats:
    """
    An evaluation answered by the solver of a site: the part of the graph it was answered on (directional or connected
    in query mode, whole_graph otherwise, alone for a graph the solver builds alone), the arguments and the attacks of
    the graph labelled, the arguments the consumers built for it, what each consumer built, and how long it took, in
    milliseconds.
    """

    locality: str
    arguments: int
    attacks: int
    added: int
    workers: list[WorkerStats]
    milliseconds: float


@dataclass(frozen=True)
class FederationStats:
    """
    A federation, as one of its sites knows it: the sites it knows, itself first.
    """

    site: str
    sites: list[SiteStats]


def _stats(stats: Any) -> FederationStats:
    def site(of: Any) -> SiteStats:
        workers = of.getWorkers()
        return SiteStats(str(of.getId()), str(of.getName()), None if workers is None else int(workers))

    return FederationStats(str(stats.getSite()), [site(it) for it in stats.getSites()])


def _evaluation(evaluation: Any) -> Optional[EvaluationStats]:
    def worker(of: Any) -> WorkerStats:
        return WorkerStats(int(of.getPartition()), str(of.getSite()), int(of.getArguments()), int(of.getAttacks()))

    if evaluation is None:
        return None
    return EvaluationStats(
        str(evaluation.getLocality()),
        int(evaluation.getArguments()),
        int(evaluation.getAttacks()),
        int(evaluation.getAdded()),
        [worker(it) for it in evaluation.getWorkers()],
        float(evaluation.getMilliseconds()),
    )


class GoalSolver:
    """
    A parallel solver as a solver of arg2p, as Arg2p is one, on the theory and the flags of the parallel solver: any goal solved as the
    standard solver solves it, one solution at a time, with the graph of each, and the answers of queries. The graph a
    goal needs is built by consumers on the threads of the parallel solver and of the sites it is federated with (in query mode,
    the part the statements it evaluates depend on); the stages of a resolution are not kept, the graph of the last one
    alone.
    """

    def __init__(self, solver: Any) -> None:
        self._solver = solver

    def solve(self, goal: str) -> Solutions:
        """The solutions of [goal], Prolog text, computed one at a time as they are asked for."""
        return Solutions(call(lambda: self._solver.solve(goal)))

    def answer(self, query: str) -> list[Answer]:
        """The statements for [query], Prolog text, labelled in, out and und (raises Arg2pError if it halts)."""
        return call(lambda: [answer_from_jvm(answer) for answer in self._solver.answer(query)])

    def graphs(self) -> list[Graph]:
        """The graph of each labelling of the theory: several for a semantics with several (preferred, stable, ...)."""
        return call(lambda: [graph_from_jvm(graph) for graph in self._solver.graphs()])

    def graph(self) -> Graph:
        """The graph of the last solution given: the one of the last stage of its resolution."""
        return call(lambda: graph_from_jvm(self._solver.graph()))

    def compact_graphs(self) -> list[CompactGraph]:
        """The graph of each labelling as arrays: for a graph too large for graphs (see CompactGraph)."""
        return call(lambda: [CompactGraph(graph) for graph in self._solver.compactGraphs()])

    def compact_graph(self) -> CompactGraph:
        """The graph of the last solution given as arrays (see CompactGraph)."""
        return CompactGraph(self._solver.compactGraph())

    def output(self, consumer: Callable[[str], Any]) -> None:
        """Gives what the solver prints to [consumer], as the solutions are given (the standard output otherwise)."""
        self._solver.setOutput(_jvm.function1(lambda printed: consumer(str(printed))))

    def stop(self) -> None:
        """
        Stops the goal being solved, from another thread: its solution waited for is a halt, at once ("The evaluation
        was stopped"), and the work of the parallel solver for it is given up; nothing is done if no goal is being solved.
        """
        self._solver.stop()


def _arguments(*given: Any, defaults: tuple[Any, ...]) -> list[Any]:
    """
    The arguments of a constructor of the JVM, up to the last one given: those left out after it take its defaults,
    those left out before it [defaults] (as the constructor has them)
    """
    last = max((i for i, it in enumerate(given) if it is not None), default=-1)
    return [given[i] if given[i] is not None else defaults[i] for i in range(last + 1)]


def _parts(parts: Optional[int]) -> Any:
    return None if parts is None else _jvm.jclass("java.lang.Integer").valueOf(parts)


class _Solver:
    """What every parallel solver does: its theory, its flags, its evaluations, its solver."""

    _parallel: Any

    def load(self, theory: str) -> None:
        """Replaces the theory with [theory], as Prolog text."""
        call(lambda: self._parallel.load(theory))

    def add(self, clause: str) -> None:
        """Adds [clause], as Prolog text, to the theory."""
        call(lambda: self._parallel.add(clause))

    def reset(self) -> None:
        """Forgets the theory."""
        call(self._parallel.reset)

    def solve(self, statement: str) -> EvaluationResponse:
        """The statements answering [statement], as Prolog text, by label, each with the arguments for it (Arg2pError for one that is not a statement)."""
        return call(lambda: _response(self._parallel.solve(statement)))

    @property
    def goals(self) -> GoalSolver:
        """The parallel solver as a solver of arg2p: any goal, its solutions and their graphs, the answers of queries."""
        return GoalSolver(self._parallel.getGoals())

    @property
    def evaluation(self) -> Optional[EvaluationStats]:
        """The last evaluation the parallel solver answered, None before the first."""
        return _evaluation(self._parallel.getEvaluation())

    @property
    def flags(self) -> Flags:
        """
        The flags of the solver, the initial ones those given: those set apply from the next evaluation on, the graph
        built anew.
        """
        return Flags(self._parallel.getFlags())

    @flags.setter
    def flags(self, flags: Flags) -> None:
        call(lambda: self._parallel.setFlags(flags.jvm()))


class ThreadedSolver(_Solver):
    """
    A distributed solver on the threads of the JVM (as many as the processors by default): the solver of its theory and
    the solvers of the parts of the theory build the graph together. The theory is split into at most [parts] partitions
    if given, as many as the consumers it can run otherwise; the graph is labelled as the standard solver does with
    [flags]. With query mode (the default) the graph is built on the arguments the statements evaluated depend on, as the
    standard solver builds it.
    """

    def __init__(
        self,
        threads: Optional[int] = None,
        parts: Optional[int] = None,
        flags: Optional[Flags] = None,
    ) -> None:
        parallel = _jvm.jclass(f"{_PARALLEL}.ThreadedSolver")
        arguments = _arguments(
            threads,
            _parts(parts),
            flags and flags.jvm(),
            defaults=(parallel.getProcessors(), None, Flags.defaults().jvm()),
        )
        self._parallel = call(lambda: parallel(*arguments))

    @property
    def id(self) -> str:
        """The identifier of the parallel solver."""
        return str(self._parallel.getId())

    def close(self) -> None:
        """Stops the threads, and closes the federation of the parallel solver, if it has one."""
        call(self._parallel.close)

    def __enter__(self) -> ThreadedSolver:
        return self

    def __exit__(self, *_: Any) -> None:
        self.close()


class Federation:
    """
    [parallel] federated with others, of the JVM, of Node.js or of browsers: it runs consumers of the graphs of the others
    if it [lends] (it does by default), and places the consumers of its graphs on them. It closes with the parallel solver, or
    before it (close), the parallel solver going on alone.
    """

    def __init__(self, parallel: ThreadedSolver, lends: bool = True) -> None:
        federation = _jvm.jclass(f"{_FEDERATION}.Federation")
        self._parallel = parallel
        self._federation = call(lambda: federation(parallel._parallel, lends))

    @property
    def id(self) -> str:
        """The identifier of the site of the parallel solver, as the others know it."""
        return self._parallel.id

    @property
    def peers(self) -> set[str]:
        """The sites the parallel solver is federated with."""
        return {str(peer) for peer in self._federation.getPeers()}

    @property
    def stats(self) -> FederationStats:
        """The federation, as the parallel solver knows it: the sites."""
        return _stats(self._federation.getStats())

    def listen(self, port: int = 0, host: str = "0.0.0.0") -> int:
        """Accepts the links of other sites over WebSockets, on [port] of [host] (an ephemeral port for 0): the port."""
        return int(call(lambda: self._federation.listen(port, host)))

    def connect(self, uri: str, code: Optional[str] = None) -> None:
        """
        Federates the parallel solver with the site listening at [uri] (ws://host:port), once it admits this one: presenting
        [code] if it requires one, the two paired then, and linked again to the same address with no code while they run
        (Arg2pError if it refuses the code, or is not there).
        """
        call(lambda: self._federation.connect(uri, code))

    @property
    def name(self) -> str:
        """The name of the site, as its peers are told: the name of its host by default."""
        return str(self._federation.getName())

    @name.setter
    def name(self, name: str) -> None:
        self._federation.setName(name)

    @property
    def requires_code(self) -> bool:
        """Whether the parallel solvers linking to this one must present its code, or a pairing made with it."""
        return bool(self._federation.getRequiresCode())

    @requires_code.setter
    def requires_code(self, required: bool) -> None:
        self._federation.setRequiresCode(required)

    @property
    def code(self) -> str:
        """The code the parallel solvers linking to this one present, if it requires one: four digits, random."""
        return str(self._federation.getCode())

    def new_code(self) -> str:
        """A new code, the one before refused from now on (the pairings made stay)."""
        return str(self._federation.newCode())

    def close(self) -> None:
        """Closes the links to the other sites: the parallel solver goes on, alone."""
        call(self._federation.close)
