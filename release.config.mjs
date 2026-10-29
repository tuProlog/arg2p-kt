var publishCmd = `
./gradlew publishAllPublicationsToProjectLocalRepository zipMavenCentralPortalPublication releaseMavenCentralPortalPublication || exit 1
./gradlew publishJsPackageToNpmjsRegistry --continue || echo "::error title=npm publication failed::Some packages were not published on npmjs, see the publishJsPackageToNpmjsRegistry tasks output"
./gradlew :plugins:pluginPackages :lsp:shadowJar || exit 1
`

// The version of the release, where the plugins for AI assistants and the MCP Registry state it (committed with the
// release, see the git plugin below): ${nextRelease.version} is resolved by semantic-release, not by node
var prepareCmd = 'node plugins/version.mjs ${nextRelease.version}'

// Runs only when a release was actually published. The back-merge step in
// release.yml keys off this file, so it must not be interpolated by JS here:
// ${nextRelease.version} is resolved by semantic-release, not by node.
var successCmd = 'echo "${nextRelease.version}" > .released-version'

import config from 'semantic-release-preconfigured-conventional-commits'  with { type: "json" };
import { files as versioned } from './plugins/version.mjs';

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
            // the packages of the extension of VS Code are attached as they are built, after the release (push.yml)
            "assets": [
                // the language server and MCP server of arg2p, a jar with every dependency in it (java -jar)
                { "path": "lsp/build/libs/arg2p-lsp-*-redist.jar" },
                // the skills and the plugin for AI assistants, for the clients that take them uploaded
                { "path": "plugins/build/distributions/*.zip" }
            ]
        }
    ],
    [
        "@semantic-release/git",
        {
            // those of the plugin by default, and the files stating the version of the plugins and of the MCP Registry
            "assets": [
                "CHANGELOG.md",
                "package.json",
                "package-lock.json",
                "npm-shrinkwrap.json",
                ...versioned,
            ]
        }
    ],
)

export default config;
