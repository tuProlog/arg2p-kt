import type { EngineError, SolutionData } from '@tuprolog/arg2p-ui/data';
import type { Answer } from '@tuprolog/arg2p-ui/js';
import { assert, describe, it } from 'vitest';
import { Bridge, type Server } from '../src/bridge.js';

const { deepEqual, equal } = assert;

// A server answering the requests of arg2p with [answers] (by method), and remembering what it was told
function server(answers: Record<string, (params: any) => unknown>) {
    const asked: { method: string; params: unknown }[] = [];
    const told: { method: string; params: unknown }[] = [];
    const fake: Server = {
        request: <T>(method: string, params: unknown) => {
            asked.push({ method, params });
            const answer = answers[method];
            return answer === undefined ? Promise.reject(new Error(`no ${method}`)) : Promise.resolve(answer(params) as T);
        },
        notify: (method, params) => void told.push({ method, params }),
    };
    return { fake, asked, told };
}

const solution: SolutionData = { result: 'yes', query: 'a', bindings: [], error: null, graph: { profile: 'aspic_plus', arguments: [], attacks: [] }, branch: 2 };

describe('Bridge', function () {
    it('should run the queries of the page on the server, under the ids the page gives them', async function () {
        const { fake, asked } = server({
            'arg2p/solve': () => ({ id: 41, query: 'a' }),
            'arg2p/next': () => ({ solution, hasNext: false }),
            'arg2p/graphAt': () => solution.graph,
        });
        const posted: Answer[] = [];
        const bridge = new Bridge(fake, answer => posted.push(answer));
        await bridge.handle({ type: 'solve', id: 1, theory: 'f :=> a.', query: 'a.', flags: '' });
        await bridge.handle({ type: 'next', id: 1 });
        await bridge.handle({ type: 'graphAt', id: 1, branch: 2, reqId: 7 });
        deepEqual(posted, [
            { type: 'started', id: 1, query: 'a' },
            { type: 'solution', id: 1, solution, hasNext: false },
            { type: 'graph', reqId: 7, graph: solution.graph },
        ]);
        deepEqual(asked.map(it => it.params), [{ theory: 'f :=> a.', query: 'a.', flags: '' }, { id: 41 }, { id: 41, branch: 2 }]);
    });

    it('should tell the failures, and forget the runs of the page', async function () {
        const error: EngineError = { name: 'Theory error', message: 'm', syntax: { line: 2, column: 9, length: 1, message: 'x', where: 'theory' } };
        const { fake, told } = server({ 'arg2p/solve': () => ({ id: 5, error }) });
        const posted: Answer[] = [];
        const bridge = new Bridge(fake, answer => posted.push(answer));
        await bridge.handle({ type: 'solve', id: 1, theory: 'a :=> b(.', query: 'q', flags: '' });
        await bridge.handle({ type: 'next', id: 9 });
        equal(posted[0].type, 'failed');
        deepEqual((posted[0] as Extract<Answer, { type: 'failed' }>).error, error);
        equal((posted[1] as Extract<Answer, { type: 'failed' }>).error.message, 'This query is no longer running.');
        bridge.dispose();
        deepEqual(told, [{ method: 'arg2p/close', params: { id: 5 } }]);
    });

    it('should keep answering when the server does not', async function () {
        const { fake } = server({});
        const posted: Answer[] = [];
        const bridge = new Bridge(fake, answer => posted.push(answer));
        await bridge.handle({ type: 'warmUp' });
        await bridge.handle({ type: 'solve', id: 1, theory: '', query: 'a', flags: '' });
        await bridge.handle({ type: 'check', reqId: 3, text: 'a.', kind: 'theory' });
        await bridge.handle({ type: 'settings', reqId: 4, theory: '', flags: '' });
        deepEqual(
            posted.map(it => it.type),
            ['warm', 'failed', 'checked', 'settings'],
        );
        equal((posted[1] as Extract<Answer, { type: 'failed' }>).error.name, 'Solver error');
    });

    it('should pass on what a run prints to the page that started it', async function () {
        const { fake } = server({ 'arg2p/solve': () => ({ id: 12, query: 'write(hi)' }) });
        const posted: Answer[] = [];
        const bridge = new Bridge(fake, answer => posted.push(answer));
        await bridge.handle({ type: 'solve', id: 3, theory: '', query: 'write(hi)', flags: '' });
        bridge.output({ id: 12, text: 'hi' });
        bridge.output({ id: 99, text: 'not mine' });
        deepEqual(posted.at(-1), { type: 'output', id: 3, text: 'hi' });
        equal(posted.filter(it => it.type === 'output').length, 1);
    });
});
