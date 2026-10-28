"""The parallel solver of arg2p: clusters of workers, run in turn (LocalCluster) or on the threads of the JVM
(ThreadedCluster), federated with other clusters (of the JVM, of Node.js, of browsers) over WebSockets, by address or
found on the local network."""

from __future__ import annotations

import os
from dataclasses import dataclass, field
from typing import Any, Optional

from . import _jvm
from .flags import Flags

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


def _parts(parts: Optional[int]) -> Any:
    return None if parts is None else _jvm.jclass("java.lang.Integer").valueOf(parts)


class LocalCluster:
    """
    A distributed solver whose nodes run in turn, on the thread of the caller: the same messages as on a cluster, in a
    deterministic order. The theory is split into at most [parts] partitions if given, one for each group of rules
    supporting one another otherwise; the graph is labelled as the standard solver does with [flags].
    """

    def __init__(self, parts: Optional[int] = None, flags: Optional[Flags] = None) -> None:
        scheduler = _jvm.jclass("it.unibo.tuprolog.argumentation.parallel.federation.SequentialScheduler")()
        self._cluster = _jvm.jclass(f"{_CLUSTER}.LocalCluster")(_parts(parts), (flags or Flags.defaults()).jvm(), scheduler, True)

    def load(self, theory: str) -> None:
        """Replaces the theory with [theory], as Prolog text."""
        _jvm.run(lambda: self._cluster.load(theory))

    def add(self, clause: str) -> None:
        """Adds [clause], as Prolog text, to the theory."""
        _jvm.run(lambda: self._cluster.add(clause))

    def reset(self) -> None:
        """Forgets the theory."""
        _jvm.run(self._cluster.reset)

    def solve(self, statement: str) -> EvaluationResponse:
        """The arguments for [statement], as Prolog text, by label."""
        return _jvm.run(lambda: _response(self._cluster.solve(statement)))


class ThreadedCluster:
    """
    A distributed solver on the threads of the JVM: each node handles its messages one at a time, the nodes in parallel
    on [threads] threads (as many as the processors by default). The theory is split into at most [parts] partitions
    if given, as many as the workers of the cluster and of the sites it is federated with otherwise; the graph is
    labelled as the standard solver does with [flags]. With query mode (the default) the graph is built on the rules
    the statements evaluated depend on. [lends] tells whether the cluster lends its workers to the others.
    """

    def __init__(
        self,
        threads: Optional[int] = None,
        parts: Optional[int] = None,
        flags: Optional[Flags] = None,
        textual: bool = False,
        lends: bool = True,
    ) -> None:
        self._cluster = _jvm.jclass(f"{_CLUSTER}.ThreadedCluster")(
            threads or os.cpu_count() or 1, _parts(parts), (flags or Flags.defaults()).jvm(), textual, lends
        )

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
        return int(self._cluster.listen(port, host))

    def discover(self, beacon_port: Optional[int] = None) -> None:
        """Federates the cluster with the clusters of the local network that discover the others as well."""
        if beacon_port is None:
            beacon_port = int(_jvm.jclass("it.unibo.tuprolog.argumentation.parallel.federation.Beacons").PORT)
        self._cluster.discover(beacon_port)

    def connect(self, uri: str) -> None:
        """Federates the cluster with the site listening at [uri] (ws://host:port)."""
        self._cluster.connect(uri)

    def load(self, theory: str) -> None:
        """Replaces the theory with [theory], as Prolog text."""
        self._cluster.load(theory)

    def add(self, clause: str) -> None:
        """Adds [clause], as Prolog text, to the theory."""
        self._cluster.add(clause)

    def reset(self) -> None:
        """Forgets the theory."""
        self._cluster.reset()

    def solve(self, statement: str) -> EvaluationResponse:
        """The arguments for [statement], as Prolog text, by label."""
        return _response(self._cluster.solve(statement))

    def close(self) -> None:
        """Closes the links to the other sites, and stops the threads."""
        self._cluster.close()

    def __enter__(self) -> ThreadedCluster:
        return self

    def __exit__(self, *_: Any) -> None:
        self.close()
