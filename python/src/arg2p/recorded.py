"""
The recorded page of the solver: a run of a query already solved, as an HTML page showing it with the solver of the
playground (its solutions, their argument graphs, the flags in effect, the theory), which solves nothing. For where
no engine runs beside the page: the sandboxes of AI assistants, whose pages start no workers (claude.ai, say).
"""

from __future__ import annotations

from typing import Any, Optional, Union

from . import _jvm
from .arg2p import call
from .flags import Flags

_PAGE = "it.unibo.tuprolog.argumentation.lsp.RecordedPage"


def defaults() -> dict[str, Any]:
    """
    What page takes when it is not given them, as the JVM has them, by the names of its arguments: the query, the flags,
    the most solutions and the timeout.
    """
    recorded = _jvm.jclass(_PAGE)
    return {
        "query": str(recorded.DEFAULT_QUERY),
        "flags": str(recorded.DEFAULT_FLAGS),
        "max_solutions": int(recorded.DEFAULT_SOLUTIONS),
        "timeout": int(recorded.DEFAULT_TIMEOUT),
    }


def page(
    theory: str,
    query: Optional[str] = None,
    flags: Union[str, Flags, None] = None,
    max_solutions: Optional[int] = None,
    timeout: Optional[int] = None,
) -> str:
    """
    The page of [query] solved on [theory] with [flags] (Prolog facts, or Flags): its first [max_solutions] solutions
    with their graphs, and the flags in effect, those found within [timeout] seconds. Those not given are the defaults
    (see defaults: the statements labelled in, out and und, with the semantics, the statement labelling and the query
    mode of arg2p). A theory that does not parse is recorded as such: the page shows the error.
    """
    given = {"query": query, "flags": None if flags is None else str(flags), "max_solutions": max_solutions, "timeout": timeout}
    stated = {**defaults(), **{name: value for name, value in given.items() if value is not None}}
    recorded = _jvm.jclass(_PAGE)
    return str(
        call(
            lambda: recorded.page(theory, stated["query"], stated["flags"], stated["max_solutions"], stated["timeout"])
        )
    )
