import { Emitter } from 'vscode-jsonrpc';
import { assert, describe, it } from 'vitest';
import { LanguageServer, type Client } from '../src/server.js';

const { deepEqual, equal } = assert;

// A client as vscode-languageclient has it: one that refuses to stop unless it runs, whose start fails with [java]
// missing, and whose stop fails with its server [stuck]; what happens to it goes to [log]
function client(log: string[], name: string, starts = true, stuck = false) {
    let state: 'initial' | 'running' | 'failed' | 'stopped' = 'initial';
    const changed = new Emitter<unknown>();
    const become = (next: typeof state) => {
        state = next;
        changed.fire({});
    };
    const fake = {
        start: async () => {
            log.push(`start ${name}`);
            become(starts ? 'running' : 'failed');
            if (!starts) throw new Error(`no ${name}`);
        },
        stop: async () => {
            if (state !== 'running') throw new Error(`Client is not running and can't be stopped. It's current state is: ${state}`);
            log.push(`stop ${name}`);
            // a server that does not answer the shutdown: the client gives up, still running
            if (stuck) throw new Error(`Stopping the server timed out`);
            become('stopped');
        },
        dispose: async () => {
            log.push(`dispose ${name}`);
            if (state === 'failed') throw new Error('Client is not running');
        },
        isRunning: () => state === 'running',
        sendRequest: async (method: string) => `${name}: ${method}`,
        onDidChangeState: changed.event,
        onNotification: () => ({ dispose: () => undefined }),
        // the server crashed, and the client restarts it
        crash: () => {
            become('stopped');
            become('running');
        },
    };
    return fake;
}

describe('LanguageServer', function () {
    it('should start afresh after a failed start, disposing of the client that failed', async function () {
        const log: string[] = [];
        const clients = [client(log, 'missing java', false), client(log, 'java')];
        const reported: string[] = [];
        const server = new LanguageServer(
            () => clients.shift() as unknown as Client,
            error => reported.push((error as Error).message),
        );
        await server.start();
        deepEqual(reported, ['no missing java']);
        await server.restart();
        deepEqual(log, ['start missing java', 'dispose missing java', 'start java']);
        equal(await server.request('arg2p/engine', {}), 'java: arg2p/engine');
    });

    it('should tell when the server running stops: restarted, stopped or crashed', async function () {
        const log: string[] = [];
        const first = client(log, 'first');
        const clients = [first, client(log, 'second')];
        const server = new LanguageServer(
            () => clients.shift() as unknown as Client,
            () => undefined,
        );
        let stops = 0;
        let starts = 0;
        server.onDidStop(() => stops++);
        server.onDidStart(() => starts++);
        await server.start();
        equal(starts, 1);
        // the client restarts the server that crashed: it is told started again (the federation view asks its state)
        first.crash();
        deepEqual([stops, starts], [1, 2]);
        await server.restart();
        deepEqual([stops, starts], [2, 3]);
        await server.stop();
        deepEqual([stops, starts], [3, 3]);
        deepEqual(log, ['start first', 'stop first', 'start second', 'stop second']);
        await server.request('arg2p/engine', {}).then(
            () => assert.fail('a server stopped answers nothing'),
            error => equal(error.message, 'The language server of Arg2P is not running.'),
        );
    });

    it('should start afresh when the server running does not stop, disposing of its client', async function () {
        const log: string[] = [];
        const clients = [client(log, 'stuck', true, true), client(log, 'java')];
        const server = new LanguageServer(
            () => clients.shift() as unknown as Client,
            () => undefined,
        );
        await server.start();
        await server.restart();
        deepEqual(log, ['start stuck', 'stop stuck', 'dispose stuck', 'start java']);
        equal(await server.request('arg2p/engine', {}), 'java: arg2p/engine');
    });

    it('should start and stop one at a time', async function () {
        const log: string[] = [];
        const clients = [client(log, 'first'), client(log, 'second'), client(log, 'third')];
        const server = new LanguageServer(
            () => clients.shift() as unknown as Client,
            () => undefined,
        );
        await Promise.all([server.start(), server.restart(), server.restart()]);
        deepEqual(log, ['start first', 'stop first', 'start second', 'stop second', 'start third']);
    });
});
