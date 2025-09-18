var publishCmd = `
./gradlew publishAllPublicationsToProjectLocalRepository zipMavenCentralPortalPublication releaseMavenCentralPortalPublication || exit 1
./gradlew publishJsPackageToNpmjsRegistry || exit 4
./gradlew -PotherMavenPassword=$GITHUB_TOKEN publishAllPublicationsToGithubRepository || true
`

import config from 'semantic-release-preconfigured-conventional-commits'  with { type: "json" };

config.plugins.push(
    [
        "@semantic-release/exec",
        {
            "publishCmd": publishCmd,
        }
    ],
    [
        "@semantic-release/github",
        {
            "assets": [
                { "path": "**/build/**/*redist*.jar" },
                { "path": "**/build/**/*full*.jar" },
                { "path": "**/build/**/*javadoc*.jar" },
                { "path": "build/**/*javadoc*.zip" }
            ]
        }
    ],
    "@semantic-release/git",
)

export default config;
