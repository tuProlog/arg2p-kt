"""The labels of the statements of a theory, from the command line: python -m arg2p theory.pl [-q query] ..."""

from __future__ import annotations

import argparse
import sys
from typing import Optional

from .arg2p import Arg2p, Arg2pError
from .flags import Flags, Profile, Semantics
from .values import text


def main(argv: Optional[list[str]] = None) -> int:
    parser = argparse.ArgumentParser(prog="arg2p", description="Evaluates a theory of arg2p.")
    parser.add_argument("theory", help="the file of the theory (- for the standard input)")
    parser.add_argument("-q", "--query", action="append", help="a statement to evaluate (all of them by default)")
    parser.add_argument("-p", "--profile", choices=[it.value for it in Profile], help="the profile of the theory")
    parser.add_argument("-s", "--semantics", choices=[it.value for it in Semantics], help="the semantics")
    arguments = parser.parse_args(argv)
    theory = sys.stdin.read() if arguments.theory == "-" else open(arguments.theory, encoding="utf-8").read()
    flags = Flags.defaults()
    if arguments.profile:
        flags.graph_build_mode(Profile(arguments.profile))
    if arguments.semantics:
        flags.argument_labelling_mode(Semantics(arguments.semantics))
    try:
        arg2p = Arg2p.of(theory, flags)
        if arguments.query:
            for query in arguments.query:
                answer = arg2p.answer(query)
                print(
                    f"{query}: in {text(answer.in_statements)}, out {text(answer.out_statements)}, "
                    f"und {text(answer.und_statements)}"
                )
        else:
            for index, graph in enumerate(arg2p.graphs()):
                if index:
                    print()
                for label in ("in", "out", "und"):
                    print(f"{label}: {', '.join(sorted(graph.conclusions(label)))}")
    except Arg2pError as error:
        print(f"arg2p: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
