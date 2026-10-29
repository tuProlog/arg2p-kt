---
name: arg2p-solve
description: Solve an Arg2P argumentation theory with the Arg2P solver and report what is accepted, rejected or undecided. Use whenever argumentation results are needed; never simulate the semantics by hand.
license: MIT
metadata:
  project: https://github.com/tuProlog/arg2p-kt
---

# Solving theories with Arg2P

This skill computes the labellings of an Arg2P theory (written with the `arg2p-formalize` skill, or given by the user)
and reports them. The solver does the reasoning: **never predict a labelling yourself**, and never report one the
solver did not give.

## The solver

Use the tools of the **Arg2P MCP server** when they are there (`solve`, `answer`, `check`, `settings`, `explore`):

| Tool | Takes | Gives |
| --- | --- | --- |
| `check` | `text`, `kind` (`theory`, the default, or `query`) | whether the text parses, and where it does not (line, column, token) |
| `answer` | `theory`, `query`, `flags`, `timeoutSeconds` | the statements matching a literal (`flies(X)`, `-guilty`) labelled `in`, `out` and `und` |
| `solve` | `theory`, `query`, `flags`, `maxSolutions`, `timeoutSeconds` | the solutions of any query (default `buildLabelSets(In, Out, Und)`), with their bindings and the argument graph of the last one: every argument with its conclusion, top rule and label, every attack with its type |
| `settings` | `theory`, `flags` | the value in effect of every flag for the theory: its profile, its semantics, ... |
| `explore` | `theory`, `query`, `flags` | shows the user the solver of the theory, to explore the results themselves (see the `arg2p-explore` skill) |

The `theory` is text, the `flags` Prolog facts (see `arg2p-formalize`); `theory`, and the `query` of `answer`, are
required (and the `text` of `check`). `timeoutSeconds` is 60 by default.

The errors are results to read, not to work around:

- A theory that does not parse, or that its profile refuses, is an **error** of `solve`, `answer`, `settings` and
  `explore`, saying why (with the line and column of a syntax error). Fix the theory or the profile; never answer in
  the solver's place.
- A query with no answer within `timeoutSeconds` is **stopped**, and the tool says so: see the sanity checks below.

Without the MCP server, the Python package does the same (it bundles its Java runtime: nothing else to install):

```bash
uvx arg2p theory.arg2p -q "flies(tweety)" -s preferred    # or: pip install arg2p, then arg2p ...
```

```python
from arg2p import Arg2p, Flags, Semantics
arg2p = Arg2p.of(theory, Flags.defaults().argument_labelling_mode(Semantics.PREFERRED))
answer = arg2p.answer("flies(tweety)")    # answer.in_statements, out_statements, und_statements
for graph in arg2p.graphs():              # one per labelling
    print(graph.conclusions("in"))
```

To install the MCP server itself, see <https://tuprolog.github.io/arg2p-kt/docs/run/#ai-assistants-mcp>.

## Steps

1. **Check** the theory parses (`check`, with the theory as its `text`). A syntax error comes with its line and
   column: fix the theory, do not guess around it.
2. **Confirm the setup** (`settings`): the profile (`graphBuildMode`) and the semantics (`argumentLabellingMode`) in
   effect must be the ones the formalization chose. A profile refuses a theory written outside its framework, with an
   error saying why: read the error, and fix the theory or the profile.
3. **Solve**:
   - one statement, or the statements matching a literal: `answer` with the literal;
   - the whole labelling, or several labellings: `solve` with `buildLabelSets(In, Out, Und)`, and `maxSolutions` high
     enough to see them all (each labelling is a solution; a final `no` means there are no more);
   - a value the theory computes: `solve` with a query on it (`answer(V)`), reading the bindings.
4. **Read the graph** of `solve` to justify the result: which argument attacks which, which attack succeeds.
5. **Offer to explore**: when the result is worth looking at (a conflict, several labellings, a surprise), offer the
   user the solver (`explore`; without the MCP server, the page of `arg2p page` in a code sandbox, or else a link to
   the web playground: see `arg2p-explore`).

## Semantics with several labellings

`answer` gives the **first labelling** only. Under `preferred`, `stable`, `complete` and the other semantics with
several labellings, use `solve` and read every solution:

- a statement **in in every labelling** is skeptically accepted;
- **in in at least one** is credulously accepted;
- report both views when they differ. `stable` may have no labelling at all.

## Interpreting the labels

- **in**: justified, it survives every attack. Report it as accepted.
- **out**: overruled, defeated by an in argument. Report it as rejected.
- **und**: undecided, a genuine deadlock. Report it as undecidable on the given knowledge, and say what is missing
  (usually a preference, or evidence). An undecided statement is an answer, not a failure.

## Sanity checks before trusting a result

- **Everything und, nothing in?** Probably missing `sup/2` preferences, or every claim encoded as mutually attacking
  premises: revisit the formalization.
- **A contested fact in and unattackable?** It was probably written as an axiom (`:->`) instead of a premise (`:=>`).
- **The arguments are not the ones the theory describes?** The profile is wrong: compare the arguments of the graph with
  the encoding.
- **A computed value missing?** The rule computing it never fired: check its premises match the facts verbatim.
- **No answer within the timeout?** The theory may not terminate (a recursive rule), or be large: give a larger
  `timeoutSeconds`, or simplify.
- **Cross-check one non-trivial case**: flip a preference or remove a premise, and confirm the labelling changes as
  expected.

## Reporting back

State the conclusion first, in natural language through the glossary of the formalization; then justify it by the
argument structure (which argument defeats which). Name the profile, the semantics and the flags used. Mention the
undecided statements explicitly, and the interpretation choices the verdict depends on.
