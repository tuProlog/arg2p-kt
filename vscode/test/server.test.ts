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

    // the answers of the engine to [request], in [session]
    const engine = (session: string, request: object) =>
        connection.sendRequest<{ answers: any[] }>('arg2p/engine', { session, request }).then(it => it.answers);

    beforeAll(async () => {
        process = spawn(javaOf('', join(__dirname, '..'), globalThis.process.env.JAVA_HOME), argumentsOf([], server!));
        connection = createMessageConnection(new StreamMessageReader(process.stdout), new StreamMessageWriter(process.stdin));
        connection.listen();
        await connection.sendRequest('initialize', { processId: null, rootUri: null, capabilities: {} });
    });

    afterAll(() => {
        connection?.dispose();
        process?.kill();
    });

    it('should solve a query, give its graph and what it prints, in a session apart', async function () {
        const answers = [
            ...(await engine('one', {
                type: 'solve',
                id: 1,
                theory: 'f1 :=> bird(tweety).\nr1 : bird(X) => flies(X).\nf2 :=> -flies(tweety).',
                query: 'buildLabelSets(In, Out, Und), write(done)',
                flags: 'queryMode.',
            })),
            ...(await engine('one', { type: 'next', id: 1 })),
        ];
        const solution = answers.find(it => it.type === 'solution');
        equal(solution?.solution.result, 'yes', JSON.stringify(answers));
        equal(solution.solution.graph.profile, 'aspic_plus');
        ok(solution.solution.graph.arguments.length >= 3);
        ok(answers.some(it => it.type === 'output' && it.id === 1 && it.text.includes('done')), JSON.stringify(answers));
        // the run 1 of a session is not that of another
        deepEqual(
            (await engine('two', { type: 'next', id: 1 })).map(it => it.type),
            ['failed'],
        );
        await engine('one', { type: 'close' });
    });

    it('should check texts and give the settings in effect', async function () {
        const [checked] = await engine('one', { type: 'check', reqId: 1, text: 'a :=> b.\nc :=> d(.', kind: 'theory' });
        deepEqual([checked.problem?.line, checked.problem?.column], [2, 9]);
        const [settings] = await engine('one', { type: 'settings', reqId: 2, theory: 'graphBuildMode(aba).', flags: '' });
        deepEqual(settings.settings.graphBuildMode, ['aba']);
    });
});

describe.skipIf(server === null)('The MCP server, as an assistant runs it', function () {
    // one JSON-RPC message a line, answers read as they come
    async function session(messages: object[]): Promise<Map<number, any>> {
        const process = spawn(javaOf('', join(__dirname, '..'), globalThis.process.env.JAVA_HOME), [...argumentsOf([], server!), '--mcp']);
        const answers = new Map<number, any>();
        let buffer = '';
        const done = new Promise<void>(resolve => {
            process.stdout.on('data', chunk => {
                buffer += chunk.toString();
                const lines = buffer.split('\n');
                buffer = lines.pop()!;
                lines.filter(it => it.trim() !== '').forEach(line => {
                    const answer = JSON.parse(line);
                    answers.set(answer.id, answer);
                });
                if (messages.filter(it => 'id' in it).every(it => answers.has((it as { id: number }).id))) resolve();
            });
        });
        messages.forEach(it => process.stdin.write(`${JSON.stringify(it)}\n`));
        await done;
        process.kill();
        return answers;
    }

    it('should list its tools and answer a query', async function () {
        const answers = await session([
            { jsonrpc: '2.0', id: 1, method: 'initialize', params: { protocolVersion: '2025-06-18', capabilities: {}, clientInfo: { name: 'test', version: '1' } } },
            { jsonrpc: '2.0', method: 'notifications/initialized' },
            { jsonrpc: '2.0', id: 2, method: 'tools/list' },
            {
                jsonrpc: '2.0',
                id: 3,
                method: 'tools/call',
                params: { name: 'answer', arguments: { theory: 'f1 :=> bird(tweety).\nr1 : bird(X) => flies(X).\nf2 :=> -flies(tweety).', query: 'flies(tweety)' } },
            },
        ]);
        equal(answers.get(1).result.serverInfo.name, 'arg2p');
        deepEqual(
            answers.get(2).result.tools.map((it: { name: string }) => it.name),
            ['solve', 'answer', 'check', 'settings', 'explore', 'engine'],
        );
        deepEqual(answers.get(3).result.structuredContent.out, ['flies(tweety)']);
    });
});
