# Arg2P for AI assistants

[Arg2P](https://tuprolog.github.io/arg2p-kt/) for AI assistants: structured argumentation (ASPIC<sup>+</sup>, ABA,
abstract frameworks) solved by its engine, not guessed. The plugin bundles:

- **skills**, the know-how of Arg2P:
  - `arg2p-formalize`: turning a problem with conflicting information, exceptions, norms, preferences or burdens of
    proof into a theory, with its profile and flags;
  - `arg2p-solve`: solving it, reading the labellings, checking them, reporting back;
  - `arg2p-explore`: showing the user the solver of the theory, to explore the results themselves;
- the **MCP server** of Arg2P, which the skills solve with: the tools `check`, `answer`, `solve`, `settings` and
  `explore`. It runs from the Python package of Arg2P with [uv](https://docs.astral.sh/uv/) (`uvx arg2p mcp`), on the
  Java runtime the package bundles.

Where the client shows MCP Apps (Claude, ChatGPT, VS Code), `explore` shows the solver in the conversation: the
solutions of the query, their argument graphs and stages, the drawing of an abstract framework, the flags. Everywhere,
it links the web playground with the same theory.

## Installing

The plugin needs [uv](https://docs.astral.sh/uv/getting-started/installation/) for its MCP server.

**Claude Code**:

```text
/plugin install arg2p --marketplace tuProlog/arg2p-kt
```

or `/plugin marketplace add tuProlog/arg2p-kt`, then `/plugin install arg2p@arg2p`.

**Claude (claude.ai, Claude Desktop)**: upload `arg2p-plugin-<version>.zip` of the
[latest release](https://github.com/tuProlog/arg2p-kt/releases/latest) in _Customize > Plugins_, or a skill alone
(`arg2p-formalize-skill-<version>.zip`, ...) in _Customize > Skills_. In chats, local MCP servers do not run: add the
server to Claude Desktop (below) for the skills to solve.

**Codex, ChatGPT**: `codex plugin marketplace add tuProlog/arg2p-kt`, then install `arg2p` from `/plugins`.

**Skills alone**, in any agent reading [Agent Skills](https://agentskills.io): `npx skills add tuProlog/arg2p-kt`.

**The MCP server alone**, in any MCP client (Claude Desktop, Cursor, ...):

```json
{
  "mcpServers": {
    "arg2p": { "command": "uvx", "args": ["arg2p", "mcp"] }
  }
}
```

It is listed in the [MCP Registry](https://registry.modelcontextprotocol.io) as `io.github.tuProlog/arg2p`. With Java
21 or later instead of uv: `java -jar arg2p-lsp-<version>-redist.jar --mcp`, the jar of the latest release.
