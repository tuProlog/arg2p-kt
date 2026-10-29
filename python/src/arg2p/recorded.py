"""
The recorded page of the solver: a run of a query already solved, as an HTML page showing it with the solver of the
playground (its solutions, their argument graphs, the flags in effect, the theory), which solves nothing. For where
no engine runs beside the page: the sandboxes of AI assistants, whose pages start no workers (claude.ai, say).
"""

from __future__ import annotations

from typing import Optional, Union

from . import _jvm
from .arg2p import call
from .flags import Flags

_PAGE = "it.unibo.tuprolog.argumentation.lsp.RecordedPage"


def page(
    theory: str,
    query: Optional[str] = None,
    flags: Union[str, Flags, None] = None,
    max_solutions: Optional[int] = None,
    timeout: Optional[int] = None,
) -> str:
    """
    The page of [query] (``buildLabelSets(In, Out, Und)`` by default) solved on [theory] with [flags] (Prolog facts, or
    Flags; the semantics, the statement labelling and the query mode of arg2p by default): its first [max_solutions]
    solutions (20 by default) with their graphs, and the flags in effect, those found within [timeout] seconds (60 by
    default). A theory that does not parse is recorded as such: the page shows the error.
    """
    recorded = _jvm.jclass(_PAGE)
    return str(
        call(
            lambda: recorded.page(
                theory,
                recorded.DEFAULT_QUERY if query is None else query,
                recorded.DEFAULT_FLAGS if flags is None else str(flags),
                recorded.DEFAULT_SOLUTIONS if max_solutions is None else max_solutions,
                recorded.DEFAULT_TIMEOUT if timeout is None else timeout,
            )
        )
    )
