"""
The link opening a theory in the web playground of Arg2P, with its query and flags, for when the MCP server of Arg2P
(and its tool explore) is not there:

    python playground_link.py theory.arg2p --query "arg2p::solve(In, Out, Und)" --flags "argumentLabellingMode(grounded).\nqueryMode."
    python playground_link.py - < theory.arg2p          (the theory on the standard input)

The playground reads the theory, the query and the flags of the plain parameters of the link (theory, query, flags),
and opens them in a tab of their own.
"""

import argparse
import sys
from urllib.parse import urlencode

PLAYGROUND = "https://tuprolog.github.io/arg2p-kt-web/"


def link(theory: str, query: str = "", flags: str = "") -> str:
    """The link to the playground opening [theory], [query] and [flags] (those not given left out)"""
    params = {name: value for name, value in (("theory", theory), ("query", query), ("flags", flags)) if value}
    return f"{PLAYGROUND}?{urlencode(params)}"


def main() -> None:
    parser = argparse.ArgumentParser(description="The link opening a theory in the web playground of Arg2P.")
    parser.add_argument("theory", help="the file of the theory (- for the standard input)")
    parser.add_argument("-q", "--query", default="", help="the query run first (the playground's default otherwise)")
    parser.add_argument("-f", "--flags", default="", help="the flags, as Prolog facts (\\n between them is a new line)")
    arguments = parser.parse_args()
    theory = sys.stdin.read() if arguments.theory == "-" else open(arguments.theory, encoding="utf-8").read()
    print(link(theory, arguments.query, arguments.flags.replace("\\n", "\n")))


if __name__ == "__main__":
    main()
