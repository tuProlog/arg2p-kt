---
name: arg2p-explore
description: Show the user the Arg2P solver of a theory, to explore its results themselves (solutions, argument graphs, stages, flags), when they want to see, browse, visualize or play with them.
license: MIT
metadata:
  project: https://github.com/tuProlog/arg2p-kt
---

# Exploring results with the solver of Arg2P

The `explore` tool of the Arg2P MCP server shows the user the solver of a theory: the query run, its solutions, the
argument graph of each and the stages of its computation, the drawing of an abstract framework, the flags and what
the theory prints. The user can change the query, the theory or the flags there and solve again.

## When

- The user asks to see, browse, visualize, explore, draw or play with the results, the arguments or the graph.
- After solving with `arg2p-solve`, when the result is worth looking at: a conflict resolved by a preference, several
  labellings, an undecided deadlock, a surprising verdict. Offer it; do not open it unasked for every small query.

## How

Call `explore` with the same `theory`, `query` and `flags` you solved with:

```json
{ "theory": "f1 :=> a.\nf2 :=> -a.\nsup(f1, f2).", "query": "arg2p::solve(In, Out, Und)", "flags": "argumentLabellingMode(grounded).\nqueryMode." }
```

- In clients that show MCP Apps (Claude, ChatGPT, VS Code), the solver appears in the conversation and runs the query.
- Everywhere, the result also carries a **link to the web playground** with the same theory, query and flags: give the
  user the link when the client shows no app (a terminal, an editor without apps).

A theory that does not parse, or that its profile refuses, is an error: fix it first (see `arg2p-solve`).

## Without the MCP server

When there is no `explore` tool (the skills installed alone, or a client without the MCP server, as claude.ai), and
you run code in a sandbox the user's browser cannot reach (claude.ai with code execution, others), make the page of
the solver with the Python package and give the user the HTML file:

```bash
pip install arg2p
arg2p page theory.arg2p --query "arg2p::solve(In, Out, Und)" --flags "argumentLabellingMode(grounded).\nqueryMode."
```

It prints the path of the page it wrote (`-o` names it; `-` reads the theory from the standard input). Use the same
theory, query and flags you solved with. claude.ai shows the file live in the conversation: the solutions, the
argument graph of each, the flags in effect and the theory, and a link to the web playground.

The page shows the run as it was solved, and does not solve again: for another query, an edited theory or other
flags, solve them and make a new page. A note tells when the solutions were cut short (`--max-solutions`, 20 by
default) or ran out of time (`--timeout`, 60 seconds).

Otherwise (no code runs, or it runs on the user's machine), give the user a link to the web playground opening the
same theory, query and flags, to solve them live. The playground reads them from the plain parameters of the link,
URL-encoded:

```text
https://tuprolog.github.io/arg2p-kt-web/?theory=<theory>&query=<query>&flags=<flags>
```

Where you can run code, `scripts/playground_link.py` makes it exactly (Python 3, nothing to install):

```bash
python scripts/playground_link.py theory.arg2p --query "arg2p::solve(In, Out, Und)" --flags "argumentLabellingMode(grounded).\nqueryMode."
```

Otherwise encode each value yourself (spaces as `%20` or `+`, new lines as `%0A`, `(` `)` `,` `:` `=` `>` `<` `%` `&`
`#` percent-encoded), and leave out what you do not set. The playground opens the theory in a tab of its own, and the
user runs it there.

## After

Where the client lets it, the solver tells you when the user solves something other than what you gave it (another
query, an edited theory or other flags): take that into account in your next answers, and solve again with
`arg2p-solve` if they ask about it.
Do not describe what the solver shows as if you had seen it: report what the tools returned to you.
