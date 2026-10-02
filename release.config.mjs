var publishCmd = `
./gradlew publishAllPublicationsToProjectLocalRepository zipMavenCentralPortalPublication releaseMavenCentralPortalPublication || exit 1
./gradlew publishJsPackageToNpmjsRegistry --continue || echo "::error title=npm publication failed::Some packages were not published on npmjs, see the publishJsPackageToNpmjsRegistry tasks output"
`

// Runs only when a release was actually published. The back-merge step in
// release.yml keys off this file, so it must not be interpolated by JS here:
// ${nextRelease.version} is resolved by semantic-release, not by node.
var successCmd = 'echo "${nextRelease.version}" > .released-version'

import config from 'semantic-release-preconfigured-conventional-commits'  with { type: "json" };

config.plugins.push(
    [
        "@semantic-release/exec",
        {
            "publishCmd": publishCmd,
            "successCmd": successCmd,
        }
    ],
    [
        "@semantic-release/github",
        {
            "assets": [
                { "path": "**/build/**/*redist*.jar" }
            ]
        }
    ],
    "@semantic-release/git",
)

export default config;
