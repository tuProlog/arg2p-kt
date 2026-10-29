"""The parallel solver of arg2p: clusters of workers, run in turn (LocalCluster) or on the threads of the JVM
(ThreadedCluster), federated with other clusters (of the JVM, of Node.js, of browsers) over WebSockets, by address or
found on the local network."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Callable, Optional

from . import _jvm
from .arg2p import Answer, Solutions, answer_from_jvm, call
from .flags import Flags
from .model import Graph, graph_from_jvm

_CLUSTER = "it.unibo.tuprolog.argumentation.parallel.cluster"


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
    """The arguments for a statement, by label."""

    in_args: list[Response] = field(default_factory=list)
    out_args: list[Response] = field(default_factory=list)
    und_args: list[Response] = field(default_factory=list)


def _response(response: Any) -> EvaluationResponse:
    def responses(of: Any) -> list[Response]:
        return [Response(str(it.getClaim()), tuple(str(argument) for argument in it.getQueryChain())) for it in of]

    return EvaluationResponse(responses(response.getInArgs()), responses(response.getOutArgs()), responses(response.getUndArgs()))


@dataclass(frozen=True)
class SiteStats:
    """
    A site of a federation: its identifier, the workers it offers (None for no limit, or for a site reached through
    another), whether it is a delegate of the site knowing it (one of its threads), and the site through which it is
    reached (via: None for the site itself and those linked to it).
    """

    id: str
    workers: Optional[int]
    delegate: bool
    via: Optional[str]


@dataclass(frozen=True)
class WorkerStats:
    """A worker of a graph: its partition of the theory, the site that ran it, the arguments it built, the attacks on them."""

    partition: int
    site: str
    arguments: int
    attacks: int


@dataclass(frozen=True)
class EvaluationStats:
    """
    An evaluation answered by the catalog of a site: the part of the graph it was answered on (directional or connected
    in query mode, whole_graph otherwise), the arguments and the attacks of the graph labelled, the arguments the workers
    built for it, what each worker built, and how long it took, in milliseconds.
    """

    locality: str
    arguments: int
    attacks: int
    added: int
    workers: list[WorkerStats]
    milliseconds: float


@dataclass(frozen=True)
class FederationStats:
    """What a federation does, as one of its sites knows it: the sites it knows (itself first), and its last evaluation."""

    site: str
    sites: list[SiteStats]
    evaluation: Optional[EvaluationStats]


def _stats(stats: Any) -> FederationStats:
    def site(of: Any) -> SiteStats:
        workers, via = of.getWorkers(), of.getVia()
        delegate = bool(of.getDelegate())
        return SiteStats(str(of.getId()), None if workers is None else int(workers), delegate, None if via is None else str(via))

    def worker(of: Any) -> WorkerStats:
        return WorkerStats(int(of.getPartition()), str(of.getSite()), int(of.getArguments()), int(of.getAttacks()))

    evaluation = stats.getEvaluation()
    return FederationStats(
        str(stats.getSite()),
        [site(it) for it in stats.getSites()],
        None
        if evaluation is None
        else EvaluationStats(
            str(evaluation.getLocality()),
            int(evaluation.getArguments()),
            int(evaluation.getAttacks()),
            int(evaluation.getAdded()),
            [worker(it) for it in evaluation.getWorkers()],
            float(evaluation.getMilliseconds()),
        ),
    )


class ClusterSolver:
    """
    A cluster as a solver of arg2p, as Arg2p is one, on the theory and the flags of the cluster: any goal solved as the
    standard solver solves it, one solution at a time, with the graph of each, and the answers of queries. The graph a
    goal needs is built by the workers of the cluster and of the sites it is federated with (in query mode, the part the
    statements it evaluates depend on); the stages of a resolution are not kept, the graph of the last one alone.
    """

    def __init__(self, solver: Any) -> None:
        self._solver = solver

    def solve(self, goal: str) -> Solutions:
        """The solutions of [goal], Prolog text, computed one at a time as they are asked for."""
        return Solutions(call(lambda: self._solver.solve(goal)))

    def answer(self, query: str) -> Answer:
        """The statements for [query], Prolog text, labelled in, out and und (raises Arg2pError if it halts)."""
        return answer_from_jvm(call(lambda: self._solver.answer(query)))

    def graphs(self) -> list[Graph]:
        """The graph of each labelling of the theory: several for a semantics with several (preferred, stable, ...)."""
        return call(lambda: [graph_from_jvm(graph) for graph in self._solver.graphs()])

    def graph(self) -> Graph:
        """The graph of the last solution given: the one of the last stage of its resolution."""
        return call(lambda: graph_from_jvm(self._solver.graph()))

    def output(self, consumer: Callable[[str], Any]) -> None:
        """Gives what the solver prints to [consumer], as the solutions are given (the standard output otherwise)."""
        self._solver.setOutput(_jvm.function1(lambda printed: consumer(str(printed))))

    def stop(self) -> None:
        """
        Stops the goal being solved, from another thread: its solution waited for is a halt, at once ("The evaluation
        was stopped"), and the work of the cluster for it is given up; nothing is done if no goal is being solved.
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


class _Cluster:
    """What every cluster does: its theory, its flags, its evaluations, its solver and its statistics."""

    _cluster: Any

    def load(self, theory: str) -> None:
        """Replaces the theory with [theory], as Prolog text."""
        call(lambda: self._cluster.load(theory))

    def add(self, clause: str) -> None:
        """Adds [clause], as Prolog text, to the theory."""
        call(lambda: self._cluster.add(clause))

    def reset(self) -> None:
        """Forgets the theory."""
        call(self._cluster.reset)

    def solve(self, statement: str) -> EvaluationResponse:
        """The arguments for [statement], as Prolog text, by label (Arg2pError for one that is not a statement)."""
        return call(lambda: _response(self._cluster.solve(statement)))

    @property
    def solver(self) -> ClusterSolver:
        """The cluster as a solver of arg2p: any goal, its solutions and their graphs, the answers of queries."""
        return ClusterSolver(self._cluster.getSolver())

    @property
    def stats(self) -> FederationStats:
        """What the federation does, as the cluster knows it: the sites, and the last evaluation it answered."""
        return _stats(self._cluster.getStats())

    @property
    def flags(self) -> Flags:
        """
        The flags of the solver, the initial ones those given: those set apply from the next evaluation on, the graph
        built anew, the links and the peers of the cluster kept.
        """
        return Flags(self._cluster.getFlags())

    @flags.setter
    def flags(self, flags: Flags) -> None:
        call(lambda: self._cluster.setFlags(flags.jvm()))


class LocalCluster(_Cluster):
    """
    A distributed solver whose nodes run in turn, on the thread of the caller: the same messages as on a cluster, in a
    deterministic order. The theory is split into at most [parts] partitions if given, one for each group of rules
    supporting one another otherwise; the graph is labelled as the standard solver does with [flags].
    """

    def __init__(self, parts: Optional[int] = None, flags: Optional[Flags] = None) -> None:
        arguments = _arguments(_parts(parts), flags and flags.jvm(), defaults=(None, None))
        self._cluster = call(lambda: _jvm.jclass(f"{_CLUSTER}.LocalCluster")(*arguments))



class ThreadedCluster(_Cluster):
    """
    A distributed solver on the threads of the JVM: each node handles its messages one at a time, the nodes in parallel
    on [threads] threads (as many as the processors by default). The theory is split into at most [parts] partitions
    if given, as many as the workers of the cluster and of the sites it is federated with otherwise; the graph is
    labelled as the standard solver does with [flags]. With query mode (the default) the graph is built on the
    arguments the statements evaluated depend on, as the standard solver builds it. [lends] tells whether the cluster
    lends its workers to the others (it does by default).
    """

    def __init__(
        self,
        threads: Optional[int] = None,
        parts: Optional[int] = None,
        flags: Optional[Flags] = None,
        lends: Optional[bool] = None,
    ) -> None:
        cluster = _jvm.jclass(f"{_CLUSTER}.ThreadedCluster")
        # the constructor takes (threads, parts, flags, textual, lends): textual is for the tests of the JVM alone
        arguments = _arguments(
            threads,
            _parts(parts),
            flags and flags.jvm(),
            None,
            lends,
            defaults=(cluster.getProcessors(), None, Flags.defaults().jvm(), False, None),
        )
        self._cluster = call(lambda: cluster(*arguments))

    @property
    def id(self) -> str:
        """The identifier of the site of the cluster, as the others know it."""
        return str(self._cluster.getId())

    @property
    def peers(self) -> set[str]:
        """The sites the cluster is federated with."""
        return {str(peer) for peer in self._cluster.getPeers()}

    def listen(self, port: int = 0, host: str = "0.0.0.0") -> int:
        """Accepts the links of other sites over WebSockets, on [port] of [host] (an ephemeral port for 0): the port."""
        return int(call(lambda: self._cluster.listen(port, host)))

    def discover(self) -> None:
        """Federates the cluster with the clusters of the local network that discover the others as well."""
        call(self._cluster.discover)

    def connect(self, uri: str) -> None:
        """Federates the cluster with the site listening at [uri] (ws://host:port)."""
        call(lambda: self._cluster.connect(uri))


    def close(self) -> None:
        """Closes the links to the other sites, and stops the threads."""
        call(self._cluster.close)

    def __enter__(self) -> ThreadedCluster:
        return self

    def __exit__(self, *_: Any) -> None:
        self.close()
