import { spawn, type ChildProcessWithoutNullStreams } from 'node:child_process';
import { existsSync, readdirSync } from 'node:fs';
import { join } from 'node:path';
import { afterAll, assert, beforeAll, describe, it } from 'vitest';
import { createMessageConnection, StreamMessageReader, StreamMessageWriter, type MessageConnection } from 'vscode-jsonrpc/node';
import { argumentsOf, javaOf } from '../src/java.js';

const { deepEqual, equal } = assert;
const ok: (value: unknown, message?: string) => void = assert.ok;

// The language server as the extension runs it: the jar of the package (server/), or the one Gradle built
function jar(): string | null {
    const packaged = join(__dirname, '..', 'server', 'arg2p-lsp.jar');
    if (existsSync(packaged)) return packaged;
    const built = join(__dirname, '..', '..', 'lsp', 'build', 'libs');
    const found = existsSync(built) ? readdirSync(built).find(it => it.endsWith('-redist.jar')) : undefined;
    return found === undefined ? null : join(built, found);
}

const server = jar();

describe.skipIf(server === null)('The language server, through JSON-RPC', function () {
    let process: ChildProcessWithoutNullStreams;
    let connection: MessageConnection;
    const printed: { id: number; text: string }[] = [];

    beforeAll(async () => {
        process = spawn(javaOf('', join(__dirname, '..'), globalThis.process.env.JAVA_HOME), argumentsOf([], server!));
        connection = createMessageConnection(new StreamMessageReader(process.stdout), new StreamMessageWriter(process.stdin));
        connection.onNotification('arg2p/output', (params: { id: number; text: string }) => void printed.push(params));
        connection.listen();
        await connection.sendRequest('initialize', { processId: null, rootUri: null, capabilities: {} });
    });

    afterAll(() => {
        connection?.dispose();
        process?.kill();
    });

    it('should solve a query, give its graph and what it prints', async function () {
        const started = await connection.sendRequest<{ id: number; query: string }>('arg2p/solve', {
            theory: 'f1 :=> bird(tweety).\nr1 : bird(X) => flies(X).\nf2 :=> -flies(tweety).',
            query: 'buildLabelSets(In, Out, Und), write(done)',
            flags: 'queryMode.',
        });
        const answer = await connection.sendRequest<{ solution: { result: string; graph: { profile: string; arguments: unknown[] } } }>('arg2p/next', {
            id: started.id,
        });
        equal(answer.solution.result, 'yes');
        equal(answer.solution.graph.profile, 'aspic_plus');
        ok(answer.solution.graph.arguments.length >= 3);
        ok(printed.some(it => it.id === started.id && it.text.includes('done')), JSON.stringify(printed));
    });

    it('should check texts and give the settings in effect', async function () {
        const problem = await connection.sendRequest<{ line: number; column: number } | null>('arg2p/check', { text: 'a :=> b.\nc :=> d(.', kind: 'theory' });
        deepEqual([problem?.line, problem?.column], [2, 9]);
        const settings = await connection.sendRequest<Record<string, string[]>>('arg2p/settings', { theory: 'graphBuildMode(aba).', flags: '' });
        deepEqual(settings.graphBuildMode, ['aba']);
    });
});
