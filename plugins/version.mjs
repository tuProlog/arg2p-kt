// The manifests of the plugin arg2p for AI assistants and of its MCP server, written from one: the plugin as
// agent-plugins.org has it (plugins/arg2p/plugin.json), whose name, description, author, homepage, repository, license,
// keywords and version the others state again, each as its client reads it, at the path it reads it from. The MCP server
// of a plugin is the one of that version of the Python package (uvx arg2p@<version> mcp), as plugins/arg2p/mcp.json
// runs it.
//
//   node plugins/version.mjs           writes the others from it
//   node plugins/version.mjs 0.20.0    states the version of a release in it, then writes the others (run by
//                                      semantic-release before its release commit, which commits them all, see
//                                      release.config.mjs)
//   node plugins/version.mjs --check   fails if one of the others is not as it writes it (./gradlew :plugins:check)
import { readFileSync, writeFileSync } from 'node:fs';

const source = 'plugins/arg2p/plugin.json';

const read = file => JSON.parse(readFileSync(file, 'utf8'));
const text = data => `${JSON.stringify(data, null, 2)}\n`;

// The fields of the plugin each manifest states as they are
const pick = (plugin, ...fields) => Object.fromEntries(fields.map(field => [field, plugin[field]]));

// The MCP servers of [data], those of the Python package run at [version]
const servers = (data, version) => ({
    ...data,
    mcpServers: Object.fromEntries(
        Object.entries(data.mcpServers).map(([name, server]) => [
            name,
            { ...server, args: server.args.map(it => it.replace(/^arg2p@.*$/, `arg2p@${version}`)) },
        ]),
    ),
});

// Each manifest written from the plugin, and from what it states of its own (written: those written before it)
const manifests = {
    // The MCP server of the plugin, for the clients of agent-plugins.org
    'plugins/arg2p/mcp.json': (plugin, data) => servers(data, plugin.version),
    // The same, for Claude Code (no $schema)
    'plugins/arg2p/.mcp.json': (plugin, data, written) => {
        const { $schema: _, ...mcp } = written['plugins/arg2p/mcp.json'];
        return mcp;
    },
    // The plugin, for Claude Code
    'plugins/arg2p/.claude-plugin/plugin.json': (plugin, data) => ({
        ...data,
        ...pick(plugin, 'name', 'version', 'description', 'author', 'homepage', 'repository', 'license', 'keywords'),
        displayName: plugin.extensions['com.openai'].interface.displayName,
    }),
    // The marketplace of the repository, for Claude Code: the plugin and its owner
    '.claude-plugin/marketplace.json': (plugin, data) => ({
        ...data,
        owner: plugin.author,
        plugins: data.plugins.map(it => ({
            ...it,
            ...pick(plugin, 'name', 'description', 'version', 'keywords', 'homepage', 'license'),
        })),
    }),
    // The marketplace of the repository, for Codex: the plugin, its name and category
    '.agents/plugins/marketplace.json': (plugin, data) => {
        const { displayName, category } = plugin.extensions['com.openai'].interface;
        return {
            ...data,
            name: plugin.name,
            interface: { ...data.interface, displayName },
            plugins: data.plugins.map(it => ({ ...it, name: plugin.name, category })),
        };
    },
    // The MCP server in the MCP Registry (with a description of its own, which the registry wants short)
    'server.json': (plugin, data) => ({
        ...data,
        title: plugin.extensions['com.openai'].interface.displayName,
        version: plugin.version,
        websiteUrl: plugin.homepage,
        repository: { ...data.repository, url: plugin.repository },
        packages: data.packages.map(it => ({ ...it, version: plugin.version })),
    }),
};

/** The files stating the version of a release: the plugin and the manifests written from it */
export const files = [source, ...Object.keys(manifests)];

/** The text of each manifest, as written from [plugin] */
export function written(plugin) {
    const data = {};
    for (const [file, write] of Object.entries(manifests)) data[file] = write(plugin, read(file), data);
    return Object.fromEntries(Object.entries(data).map(([file, it]) => [file, text(it)]));
}

if (import.meta.main) {
    const [argument] = process.argv.slice(2);
    const plugin = read(source);
    if (argument === '--check') {
        const stale = Object.entries(written(plugin)).filter(([file, it]) => readFileSync(file, 'utf8') !== it);
        if (stale.length > 0) {
            console.error(`Not as ${source} states (write them with node plugins/version.mjs): ${stale.map(([file]) => file).join(', ')}`);
            process.exit(1);
        }
        console.log(`The manifests are those of ${source}`);
    } else {
        if (argument !== undefined) {
            if (!/^\d+\.\d+\.\d+$/.test(argument)) {
                console.error(`Not the version of a release: ${argument}`);
                process.exit(1);
            }
            plugin.version = argument;
            writeFileSync(source, text(plugin));
        }
        Object.entries(written(plugin)).forEach(([file, it]) => writeFileSync(file, it));
        console.log(`The manifests state ${source}, at ${plugin.version}`);
    }
}
