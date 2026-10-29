// Everything a release publishes, built before any of it is: the version of the release written where the plugins for
// AI assistants and the MCP Registry state it (committed with the release, see the git plugin below), then the Maven
// publications (signed, in the repository of the project), the npm package, the plugins and the language server, at the
// version of the release (the build reads it from its tag otherwise, which is not there yet). ${nextRelease.version} is
// resolved by semantic-release, not by node.
var prepareCmd = [
    'node plugins/version.mjs ${nextRelease.version} || exit 1',
    './gradlew -PforceVersion=${nextRelease.version} publishAllPublicationsToProjectLocalRepository zipMavenCentralPortalPublication assembleJsPackage :plugins:pluginPackages :lsp:shadowJar || exit 1',
].join('\n')

// The uploads alone, of what prepareCmd built, at the version of the tag of the release: a failed upload fails the
// release, so that nothing is published after it (the packages of VS Code and of PyPI, see push.yml, whose phone page
// loads the npm package of the same version)
var publishCmd = [
    './gradlew publishAllPublicationsToProjectLocalRepository zipMavenCentralPortalPublication releaseMavenCentralPortalPublication || exit 1',
    './gradlew publishJsPackageToNpmjsRegistry || exit 1',
].join('\n')

// Runs only when a release was actually published. The back-merge step in
// release.yml keys off this file, so it must not be interpolated by JS here:
// ${nextRelease.version} is resolved by semantic-release, not by node.
var successCmd = 'echo "${nextRelease.version}" > .released-version'

import config from 'semantic-release-preconfigured-conventional-commits'  with { type: "json" };
import { files as versioned } from './plugins/version.mjs';

// The release notes (and the changelog) without the merges, as the back-merges of master into develop (chore: merge
// master into develop after 0.22.0), and with a change once when it came twice, as a commit and as the squash of its
// pull request (the subject of the commit, then (#559))
const notes = config.plugins.find(it => Array.isArray(it) && it[0] === '@semantic-release/release-notes-generator')[1];
const pullRequest = /\s*\((\[#\d+\]\([^)]*\)|#\d+)\)$/;
notes.presetConfig.ignoreCommits = /^chore(\([^)]*\))?: merge /;
notes.writerOpts = {
    finalizeContext: context => ({
        ...context,
        commitGroups: context.commitGroups.map(group => {
            const key = commit => `${commit.scope ?? ''}: ${(commit.subject ?? '').replace(pullRequest, '')}`;
            const squashed = new Set(group.commits.filter(it => pullRequest.test(it.subject ?? '')).map(key));
            return { ...group, commits: group.commits.filter(it => pullRequest.test(it.subject ?? '') || !squashed.has(key(it))) };
        }),
    }),
};

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
