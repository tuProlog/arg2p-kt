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
{ "theory": "f1 :=> a.\nf2 :=> -a.\nsup(f1, f2).", "query": "buildLabelSets(In, Out, Und)", "flags": "argumentLabellingMode(grounded).\nqueryMode." }
```

- In clients that show MCP Apps (Claude, ChatGPT, VS Code), the solver appears in the conversation and runs the query.
- Everywhere, the result also carries a **link to the web playground** with the same theory, query and flags: give the
  user the link when the client shows no app (a terminal, an editor without apps).

A theory that does not parse is refused: fix it first (see `arg2p-solve`).

## After

Where the client lets it, the solver tells you when the user solves something other than what you gave it (another
query, an edited theory or other flags): take that into account in your next answers, and solve again with
`arg2p-solve` if they ask about it.
Do not describe what the solver shows as if you had seen it: report what the tools returned to you.
