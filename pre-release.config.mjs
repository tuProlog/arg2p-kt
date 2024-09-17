import config from 'semantic-release-preconfigured-conventional-commits'  assert { type: "json" };

config.plugins.push(
    [
        "@semantic-release/exec",
        {
            "verifyReleaseCmd": "echo ${nextRelease.version} > .next-version",
        }
    ]
)

export default config;