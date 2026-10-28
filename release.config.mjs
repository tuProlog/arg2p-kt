var publishCmd = `
./gradlew publishAllPublicationsToProjectLocalRepository zipMavenCentralPortalPublication releaseMavenCentralPortalPublication || exit 1
./gradlew publishJsPackageToNpmjsRegistry --continue || echo "::error title=npm publication failed::Some packages were not published on npmjs, see the publishJsPackageToNpmjsRegistry tasks output"
./gradlew :plugins:pluginPackages || exit 1
`

// The version of the release, where the plugins for AI assistants and the MCP Registry state it (committed with the
// release, see the git plugin below): ${nextRelease.version} is resolved by semantic-release, not by node
var prepareCmd = 'node plugins/version.mjs ${nextRelease.version}'

// Runs only when a release was actually published. The back-merge step in
// release.yml keys off this file, so it must not be interpolated by JS here:
// ${nextRelease.version} is resolved by semantic-release, not by node.
var successCmd = 'echo "${nextRelease.version}" > .released-version'

import config from 'semantic-release-preconfigured-conventional-commits'  with { type: "json" };

config.plugins.push(
    [
        "@semantic-release/exec",
        {
            "prepareCmd": prepareCmd,
            "publishCmd": publishCmd,
            "successCmd": successCmd,
        }
    ],
    [
        "@semantic-release/github",
        {
            "assets": [
                { "path": "**/build/**/*redist*.jar" },
                // the skills and the plugin for AI assistants, for the clients that take them uploaded
                { "path": "plugins/build/distributions/*.zip" }
            ]
        }
    ],
    [
        "@semantic-release/git",
        {
            // those of the plugin by default, and the versions of the plugins and of the MCP Registry
            "assets": [
                "CHANGELOG.md",
                "package.json",
                "package-lock.json",
                "npm-shrinkwrap.json",
                ".claude-plugin/marketplace.json",
                "plugins/arg2p/.claude-plugin/plugin.json",
                "plugins/arg2p/plugin.json",
                "plugins/arg2p/.mcp.json",
                "plugins/arg2p/mcp.json",
                "server.json",
            ]
        }
    ],
)

export default config;
