"""
Arg2P from Python: structured argumentation in Prolog (ASPIC+, ABA, abstract frameworks, ...), the library of the JVM
run through JPype, on the Java runtime the package bundles.

    >>> from arg2p import Arg2p
    >>> Arg2p.of("r1 : [] => a.\nr2 : [] => -a.\nsup(r1, r2).").answer("a").in_statements
    ['a']
"""

from .arg2p import Answer, Arg2p, Arg2pError, Builder, Causality, Solution, Solutions, SyntaxProblem
from .flags import (
    ConflictModel,
    Flags,
    GraphBuilder,
    GraphExtension,
    GraphLanguage,
    OrderingComparator,
    OrderingPrinciple,
    Profile,
    Semantics,
    StatementLabelling,
)
from .model import Argument, Attack, Graph, LabelledArgument, Support
from .parallel import EvaluationResponse, LocalCluster, Response, ThreadedCluster
from .values import Struct, Value, Var

try:
    from ._version import __version__
except ImportError:  # a package built without Gradle
    __version__ = "0.0.0"

__all__ = [
    "Answer",
    "Arg2p",
    "Arg2pError",
    "Argument",
    "Attack",
    "Builder",
    "Causality",
    "ConflictModel",
    "EvaluationResponse",
    "Flags",
    "Graph",
    "GraphBuilder",
    "GraphExtension",
    "GraphLanguage",
    "LabelledArgument",
    "LocalCluster",
    "OrderingComparator",
    "OrderingPrinciple",
    "Profile",
    "Response",
    "Semantics",
    "Solution",
    "Solutions",
    "StatementLabelling",
    "Struct",
    "Support",
    "SyntaxProblem",
    "ThreadedCluster",
    "Value",
    "Var",
    "__version__",
]
