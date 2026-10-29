"""
The labels of the statements of a theory, from the command line: python -m arg2p theory.pl [-q query] ...

``arg2p mcp`` runs the MCP server of arg2p instead, for AI assistants: over the standard input and output, on the Java
runtime of the package. ``arg2p page`` writes the recorded page of a query solved on a theory, which shows the run
(see recorded.py).
"""

from __future__ import annotations

import argparse
import os
import subprocess
import sys
from typing import Optional

from . import _jvm, recorded
from .arg2p import Arg2p, Arg2pError
from .flags import Flags, Profile, Semantics
from .values import text


def main(argv: Optional[list[str]] = None) -> int:
    arguments = sys.argv[1:] if argv is None else argv
    if arguments[:1] == ["mcp"]:
        return mcp(arguments[1:])
    if arguments[:1] == ["page"]:
        return page(arguments[1:])
    return evaluate(arguments)


def mcp(argv: list[str]) -> int:
    """Runs the MCP server of arg2p until its client closes the standard input (a theory named mcp: ./mcp)"""
    argparse.ArgumentParser(prog="arg2p mcp", description="Runs the MCP server of arg2p, for AI assistants.").parse_args(argv)
    try:
        from ._version import MCP_SERVER
    except ImportError:  # a package built without Gradle, or before the server was written into it
        print("arg2p: no MCP server in the package: write it with ./gradlew :python:pythonVersion", file=sys.stderr)
        return 1
    command = [_jvm.java(), *_jvm.options(), "-cp", os.pathsep.join(_jvm.classpath()), MCP_SERVER, "--mcp"]
    try:
        return subprocess.call(command)
    except KeyboardInterrupt:
        return 130


def page(argv: list[str]) -> int:
    """
    Writes the recorded page of a query solved on a theory (a theory named page: ./page), and prints where: in the
    file of -o, or arg2p-<the name of the theory>.html (arg2p-page.html for the standard input)
    """
    parser = argparse.ArgumentParser(
        prog="arg2p page",
        description="Writes an HTML page showing a query solved on a theory: its solutions, their argument graphs, the "
        "flags in effect and the theory. The page solves nothing: for another query, or another theory, make another.",
    )
    parser.add_argument("theory", help="the file of the theory (- for the standard input)")
    parser.add_argument("-q", "--query", help="the query (default: buildLabelSets(In, Out, Und))")
    parser.add_argument(
        "-f", "--flags", help="the flags, as Prolog facts, \\n between them a new line (default: those of arg2p)"
    )
    parser.add_argument("-o", "--output", help="the file of the page (default: arg2p-<theory>.html)")
    parser.add_argument("--max-solutions", type=int, help="how many solutions at most (default: 20)")
    parser.add_argument("--timeout", type=int, help="how long to solve, in seconds (default: 60)")
    arguments = parser.parse_args(argv)
    theory = sys.stdin.read() if arguments.theory == "-" else open(arguments.theory, encoding="utf-8").read()
    name = "page" if arguments.theory == "-" else os.path.splitext(os.path.basename(arguments.theory))[0]
    output = arguments.output or f"arg2p-{name}.html"
    flags = None if arguments.flags is None else arguments.flags.replace("\\n", "\n")
    try:
        html = recorded.page(theory, arguments.query, flags, arguments.max_solutions, arguments.timeout)
    except Arg2pError as error:
        print(f"arg2p: {error}", file=sys.stderr)
        return 1
    with open(output, "w", encoding="utf-8") as file:
        file.write(html)
    print(os.path.abspath(output))
    return 0


def evaluate(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(
        prog="arg2p",
        description="Evaluates a theory of arg2p.",
        epilog="arg2p mcp runs the MCP server of arg2p instead, for AI assistants (see arg2p mcp --help); arg2p page "
        "writes an HTML page showing a query solved on a theory (see arg2p page --help). A theory in a file named mcp "
        "or page is ./mcp, ./page.",
    )
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
