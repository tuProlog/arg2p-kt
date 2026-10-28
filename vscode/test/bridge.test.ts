import { EMPTY_GRAPH, type SolutionData } from '@tuprolog/arg2p-ui/data';
import type { Answer } from '@tuprolog/arg2p-ui/js';
import { assert, describe, it } from 'vitest';
import { Bridge, type Server } from '../src/bridge.js';
import type { EngineRequest } from '../src/protocol.js';

const { deepEqual, equal, notEqual } = assert;

interface Asked {
    session: string;
    request: EngineRequest | { type: 'close' };
}

// A server answering arg2p/engine with [answers] (by the type of the request), and remembering what it was asked
function server(answers: Partial<Record<string, (request: any) => Answer[]>>) {
    const asked: Asked[] = [];
    const fake: Server = {
        request: <T>(method: string, params: unknown) => {
            equal(method, 'arg2p/engine');
            const { session, request } = params as Asked;
            asked.push({ session, request });
            const answer = answers[request.type];
            return answer === undefined ? Promise.reject(new Error(`no ${request.type}`)) : Promise.resolve({ answers: answer(request) } as T);
        },
    };
    return { fake, asked };
}

const solution: SolutionData = { result: 'yes', query: 'a', bindings: [], error: null, graph: { profile: 'aspic_plus', arguments: [], attacks: [] }, branch: 2 };

describe('Bridge', function () {
    it('should pass the requests of the page on to the server, in a session of its own, and the answers back', async function () {
        const { fake, asked } = server({
            solve: ({ id }) => [
                { type: 'output', id, text: 'hi' },
                { type: 'started', id, query: 'a' },
            ],
            next: ({ id }) => [{ type: 'solution', id, solution, hasNext: false }],
        });
        const posted: Answer[] = [];
        const bridge = new Bridge(fake, answer => posted.push(answer));
        const solve: EngineRequest = { type: 'solve', id: 1, theory: 'f :=> a.', query: 'a.', flags: '' };
        await bridge.handle(solve);
        await bridge.handle({ type: 'next', id: 1 });
        deepEqual(posted, [
            { type: 'output', id: 1, text: 'hi' },
            { type: 'started', id: 1, query: 'a' },
            { type: 'solution', id: 1, solution, hasNext: false },
        ]);
        deepEqual(asked[0].request, solve);
        equal(asked[0].session, asked[1].session);
        const other = server({ solve: () => [] });
        await new Bridge(other.fake, () => undefined).handle(solve);
        notEqual(other.asked[0].session, asked[0].session);
    });

    it('should keep answering when the server does not', async function () {
        const { fake } = server({});
        const posted: Answer[] = [];
        const bridge = new Bridge(fake, answer => posted.push(answer));
        await bridge.handle({ type: 'warmUp' });
        await bridge.handle({ type: 'solve', id: 1, theory: '', query: 'a', flags: '' });
        await bridge.handle({ type: 'graphAt', id: 1, branch: 0, reqId: 2 });
        await bridge.handle({ type: 'check', reqId: 3, text: 'a.', kind: 'theory' });
        await bridge.handle({ type: 'settings', reqId: 4, theory: '', flags: '' });
        await bridge.handle({ type: 'stop' });
        deepEqual(posted, [
            { type: 'warm' },
            { type: 'failed', id: 1, error: { name: 'Solver error', message: 'no solve', syntax: null } },
            { type: 'graph', reqId: 2, graph: EMPTY_GRAPH },
            { type: 'checked', reqId: 3, problem: null },
            { type: 'settings', reqId: 4, settings: {} },
        ]);
    });

    it('should start a session afresh once the server stopped, and drop the answers of the one before', async function () {
        let answer: (answers: { answers: Answer[] }) => void = () => undefined;
        const asked: Asked[] = [];
        const posted: Answer[] = [];
        const bridge = new Bridge(
            {
                request: <T>(_: string, params: unknown) => {
                    asked.push(params as Asked);
                    return new Promise<T>(resolve => (answer = resolve as typeof answer));
                },
            },
            it => posted.push(it),
        );
        const solving = bridge.handle({ type: 'solve', id: 1, theory: '', query: 'a', flags: '' });
        bridge.renew();
        answer({ answers: [{ type: 'started', id: 1, query: 'a' }] });
        await solving;
        deepEqual(posted, []);
        const next = bridge.handle({ type: 'next', id: 1 });
        answer({ answers: [{ type: 'failed', id: 1, error: { name: 'Error', message: 'gone', syntax: null } }] });
        await next;
        equal(posted.length, 1);
        notEqual(asked[1].session, asked[0].session);
    });

    it('should stop and close its session as the page closes, a run the server starts afterwards too', async function () {
        let started: (answers: { answers: Answer[] }) => void = () => undefined;
        const asked: Asked[] = [];
        const posted: Answer[] = [];
        const bridge = new Bridge(
            {
                request: <T>(_: string, params: unknown) => {
                    asked.push(params as Asked);
                    if ((params as Asked).request.type !== 'solve') return Promise.resolve({ answers: [] } as T);
                    return new Promise<T>(resolve => (started = resolve as typeof started));
                },
            },
            it => posted.push(it),
        );
        const solving = bridge.handle({ type: 'solve', id: 1, theory: '', query: 'a', flags: '' });
        bridge.dispose();
        started({ answers: [{ type: 'started', id: 1, query: 'a' }] });
        await solving;
        deepEqual(posted, []);
        deepEqual(
            asked.map(it => it.request.type),
            ['solve', 'stop', 'close', 'stop', 'close'],
        );
        equal(new Set(asked.map(it => it.session)).size, 1);
    });
});
