import { runTests } from '@vscode/test-electron';
import * as esbuild from 'esbuild';
import { mkdtempSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

// The extension in a VS Code of its own (user data and extensions apart, so that none of the user's is touched): the
// one of VSCODE_EXECUTABLE, or else one test-electron downloads. The extension needs its bundles and its server
// (npm run build, ./gradlew :vscode:vscodeServer).

const extension = join(dirname(fileURLToPath(import.meta.url)), '..', '..');
await esbuild.build({
    entryPoints: [join(extension, 'test', 'integration', 'suite.ts')],
    outfile: join(extension, 'dist-test', 'suite.js'),
    bundle: true,
    platform: 'node',
    format: 'cjs',
    external: ['vscode'],
    logLevel: 'warning',
});

await runTests({
    vscodeExecutablePath: process.env.VSCODE_EXECUTABLE,
    extensionDevelopmentPath: extension,
    extensionTestsPath: join(extension, 'dist-test', 'suite.js'),
    launchArgs: [
        '--disable-extensions',
        '--disable-workspace-trust',
        '--user-data-dir',
        mkdtempSync(join(tmpdir(), 'arg2p-vscode-user-')),
        '--extensions-dir',
        mkdtempSync(join(tmpdir(), 'arg2p-vscode-extensions-')),
    ],
});
