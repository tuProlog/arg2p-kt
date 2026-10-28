"""The flags of an evaluation (see the flags reference of arg2p), as FlagsBuilder takes them."""

from __future__ import annotations

from dataclasses import dataclass, fields
from typing import Any, Iterable, Optional

from . import _jvm


@dataclass
class Flags:
    """
    The flags of an evaluation, those of FlagsBuilder: a flag left None keeps its default, or the value the profile
    of the theory (graph_build_mode) presets. The theory may state flags as well, which override these.
    """

    graph_build_mode: Optional[str] = None
    argument_labelling_mode: Optional[str] = None
    statement_labelling_mode: Optional[str] = None
    ordering_principle: Optional[str] = None
    ordering_comparator: Optional[str] = None
    graph_extensions: Optional[Iterable[str]] = None
    graph_builder: Optional[str] = None
    query_mode: Optional[bool] = None
    auto_transposition: Optional[bool] = None
    prolog_strict_compatibility: Optional[bool] = None
    meta_rules: Optional[bool] = None
    meta_conflicts: Optional[bool] = None
    modules_path: Optional[str] = None

    def builder(self) -> Any:
        """The FlagsBuilder of the flags."""
        builder = _jvm.jclass("it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder")()
        for flag in fields(self):
            value = getattr(self, flag.name)
            if value is None:
                continue
            if flag.name == "graph_extensions":
                value = _jvm.jclass("java.util.ArrayList")(list(value))
            setter = "set" + "".join(part.capitalize() for part in flag.name.split("_"))
            getattr(builder, setter)(value)
        return builder
