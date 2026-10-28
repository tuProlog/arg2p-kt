"""The labels of the statements of a theory, from the command line: python -m arg2p theory.pl [-q query] ..."""

from __future__ import annotations

import argparse
import sys
from typing import Optional

from .flags import Flags
from .solver import Arg2pError, Solver


def main(argv: Optional[list[str]] = None) -> int:
    parser = argparse.ArgumentParser(prog="arg2p", description="Evaluates a theory of arg2p.")
    parser.add_argument("theory", help="the file of the theory (- for the standard input)")
    parser.add_argument("-q", "--query", action="append", help="a statement to evaluate (all of them by default)")
    parser.add_argument("-p", "--profile", help="the profile of the theory (graphBuildMode): aspic_plus, aba, ...")
    parser.add_argument("-s", "--semantics", help="the semantics (argumentLabellingMode): grounded, preferred, ...")
    arguments = parser.parse_args(argv)
    theory = sys.stdin.read() if arguments.theory == "-" else open(arguments.theory, encoding="utf-8").read()
    try:
        solver = Solver(theory, Flags(graph_build_mode=arguments.profile, argument_labelling_mode=arguments.semantics))
        if arguments.query:
            for query in arguments.query:
                answer = solver.answer(query)
                print(f"{query}: in {_text(answer.in_)}, out {_text(answer.out)}, und {_text(answer.und)}")
        else:
            for index, graph in enumerate(solver.graphs()):
                if index:
                    print()
                for label in ("in", "out", "und"):
                    print(f"{label}: {', '.join(sorted(graph.conclusions(label)))}")
    except Arg2pError as error:
        print(f"arg2p: {error}", file=sys.stderr)
        return 1
    return 0


def _text(values: list) -> str:
    return "[" + ", ".join(map(str, values)) + "]"


if __name__ == "__main__":
    sys.exit(main())
