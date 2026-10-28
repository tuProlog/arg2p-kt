"""
The labels of the statements of a theory, from the command line: python -m arg2p theory.pl [-q query] ...

``arg2p mcp`` runs the MCP server of arg2p instead, for AI assistants: over the standard input and output, on the Java
runtime of the package.
"""

from __future__ import annotations

import argparse
import os
import subprocess
import sys
from typing import Optional

from . import _jvm
from .arg2p import Arg2p, Arg2pError
from .flags import Flags, Profile, Semantics
from .values import text


def main(argv: Optional[list[str]] = None) -> int:
    arguments = sys.argv[1:] if argv is None else argv
    if arguments[:1] == ["mcp"]:
        return mcp(arguments[1:])
    return evaluate(arguments)


def mcp(argv: list[str]) -> int:
    """Runs the MCP server of arg2p until its client closes the standard input (a theory named mcp: ./mcp)"""
    argparse.ArgumentParser(prog="arg2p mcp", description="Runs the MCP server of arg2p, for AI assistants.").parse_args(argv)
    command = [java(), *_jvm.options(), "-cp", os.pathsep.join(_jvm.classpath()), MCP_SERVER, "--mcp"]
    try:
        return subprocess.call(command)
    except KeyboardInterrupt:
        return 130


#: The main class of the language server of arg2p, which is its MCP server with --mcp
MCP_SERVER = "it.unibo.tuprolog.argumentation.lsp.MainKt"


def java() -> str:
    """The java of the runtime the package bundles, or of JAVA_HOME, or the one on the path"""
    executable = "java.exe" if sys.platform == "win32" else "java"
    for home in (_jvm.RUNTIME, os.environ.get("JAVA_HOME")):
        if home and os.path.exists(os.path.join(home, "bin", executable)):
            return os.path.join(home, "bin", executable)
    return "java"


def evaluate(argv: list[str]) -> int:
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
