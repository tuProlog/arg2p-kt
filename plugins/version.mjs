// The version of a release, written where the plugins and the MCP Registry state it: node plugins/version.mjs 0.20.0
// (run by semantic-release before its release commit, see release.config.mjs). The MCP server of a plugin is the one
// of that version of the Python package (uvx arg2p@<version> mcp).
import { readFileSync, writeFileSync } from 'node:fs';

const [version] = process.argv.slice(2);
if (!/^\d+\.\d+\.\d+$/.test(version ?? '')) {
    console.error(`Not the version of a release: ${version}`);
    process.exit(1);
}

const servers = data => {
    Object.values(data.mcpServers).forEach(server => {
        server.args = server.args.map(it => it.replace(/^arg2p@.*$/, `arg2p@${version}`));
    });
};

const edits = {
    'plugins/arg2p/.claude-plugin/plugin.json': data => (data.version = version),
    'plugins/arg2p/plugin.json': data => (data.version = version),
    'plugins/arg2p/.mcp.json': servers,
    'plugins/arg2p/mcp.json': servers,
    '.claude-plugin/marketplace.json': data => data.plugins.forEach(it => (it.version = version)),
    'server.json': data => {
        data.version = version;
        data.packages.forEach(it => (it.version = version));
    },
};

for (const [file, edit] of Object.entries(edits)) {
    const data = JSON.parse(readFileSync(file, 'utf8'));
    edit(data);
    writeFileSync(file, `${JSON.stringify(data, null, 2)}\n`);
}
console.log(`The plugins and the MCP Registry state ${version}`);
