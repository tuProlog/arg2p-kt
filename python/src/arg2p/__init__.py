"""
Arg2P from Python: structured argumentation in Prolog (ASPIC+, ABA, abstract frameworks, ...), the library of the JVM
run through JPype (a JVM, Java 17 or later, is needed).

    >>> from arg2p import Solver
    >>> Solver("r1 : [] => a.\nr2 : [] => -a.\nsup(r1, r2).").answer("a").in_
    ['a']
"""

from .flags import Flags
from .model import Argument, Attack, Graph, Support
from .parallel import Cluster, Evaluated, Evaluation
from .solver import Answer, Arg2pError, Solver, answer_query, evaluate
from .terms import Struct, Var

__all__ = [
    "Answer",
    "Arg2pError",
    "Argument",
    "Attack",
    "Cluster",
    "Evaluated",
    "Evaluation",
    "Flags",
    "Graph",
    "Solver",
    "Struct",
    "Support",
    "Var",
    "answer_query",
    "evaluate",
]
