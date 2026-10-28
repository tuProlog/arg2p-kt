"""The parallel solver of arg2p: clusters of workers on the threads of the JVM, federated with other clusters (of the
JVM, of Node.js, of browsers) over WebSockets, by address or found on the local network."""

from __future__ import annotations

import os
from dataclasses import dataclass, field
from typing import Any, Optional

from . import _jvm
from .flags import Flags


@dataclass(frozen=True)
class Evaluated:
    """
    An argument for a statement: the statement it concludes, and the argument, as Prolog text (None for a statement
    with no argument, und)
    """

    claim: str
    argument: Optional[str]


@dataclass(frozen=True)
class Evaluation:
    """The arguments for a statement labelled in, out and und (in_, as in is a keyword of Python)."""

    in_: list[Evaluated] = field(default_factory=list)
    out: list[Evaluated] = field(default_factory=list)
    und: list[Evaluated] = field(default_factory=list)


class Cluster:
    """
    A distributed solver whose workers run on [threads] threads (as many as the processors by default): the graph of
    its theory is built by its workers and by those of the clusters it is federated with, then labelled as the standard
    solver does with [flags]. With query mode (the default), the graph is built on the rules the statements evaluated
    depend on. The theory is split into [parts] partitions if given; [lends] tells whether the cluster lends its
    workers to the others.
    """

    def __init__(
        self,
        threads: Optional[int] = None,
        parts: Optional[int] = None,
        flags: Optional[Flags] = None,
        lends: bool = True,
    ) -> None:
        settings = (flags or Flags()).builder()
        boxed = None if parts is None else _jvm.jclass("java.lang.Integer").valueOf(parts)
        self._cluster = _jvm.jclass("it.unibo.tuprolog.argumentation.parallel.cluster.ThreadedCluster")(
            threads or os.cpu_count() or 1, boxed, settings, False, lends
        )

    def load(self, theory: str) -> None:
        """Replaces the theory with [theory], Prolog text."""
        self._cluster.load(theory)

    def add(self, clause: str) -> None:
        """Adds [clause], Prolog text, to the theory."""
        self._cluster.add(clause)

    def reset(self) -> None:
        """Forgets the theory."""
        self._cluster.reset()

    def solve(self, statement: str) -> Evaluation:
        """The arguments for [statement], Prolog text, by label."""
        response = self._cluster.solve(statement.strip().removesuffix("."))

        def evaluated(responses: Any) -> list[Evaluated]:
            return [
                Evaluated(str(r.getClaim()), str(chain) if chain is not None else None)
                for r in responses
                for chain in (list(r.getQueryChain()) or [None])
            ]

        return Evaluation(evaluated(response.getInArgs()), evaluated(response.getOutArgs()), evaluated(response.getUndArgs()))

    def listen(self, port: int = 0, host: str = "0.0.0.0") -> int:
        """Accepts the links of other clusters on [port] of [host] (an ephemeral port for 0): the port taken."""
        return int(self._cluster.listen(port, host))

    def connect(self, url: str) -> None:
        """Federates the cluster with the one listening at [url] (ws://host:port)."""
        self._cluster.connect(url)

    def discover(self, port: int = 9430) -> None:
        """Finds the clusters of the local network discovering as well, through beacons on [port], and links to them."""
        self._cluster.discover(port)

    @property
    def site(self) -> str:
        """The identifier of the site of the cluster."""
        return str(self._cluster.getSite().getId())

    @property
    def peers(self) -> set[str]:
        """The sites the cluster is federated with."""
        return {str(peer) for peer in self._cluster.getSite().getPeerSites()}

    def close(self) -> None:
        """Closes the links to the other clusters, and stops the threads."""
        self._cluster.close()

    def __enter__(self) -> Cluster:
        return self

    def __exit__(self, *_: Any) -> None:
        self.close()
