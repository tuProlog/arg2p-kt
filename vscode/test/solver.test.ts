import type { EngineError } from '@tuprolog/arg2p-ui/data';
import { assert, describe, it } from 'vitest';
import type { FromPage, ToPage } from '../src/protocol.js';
import { connect, STOPPED, type Solver, type Switch } from '../webview/solver.js';

const { deepEqual, equal } = assert;

// The solver of the page as the extension sees it: what it is given, and what it tells (the events of <arg2p-solver>)
class FakeSolver extends EventTarget implements Solver {
    engine: Solver['engine'] = null;
    theory = '';
    query = '';
    flags = '';
    fileName = 'theory';

    tell(type: string, detail: object): void {
        Object.assign(this, detail);
        this.dispatchEvent(new CustomEvent(type, { detail }));
    }
}

// A page bound to a fake extension: what it posts, and how the extension speaks to it
function page() {
    const solver = new FakeSolver();
    const posted: FromPage[] = [];
    const window = new EventTarget();
    const parallel: Switch = Object.assign(new EventTarget(), { checked: false });
    const engine = connect(solver, { postMessage: message => posted.push(message) }, window, parallel);
    const send = (message: ToPage) => window.dispatchEvent(new MessageEvent('message', { data: message }));
    return { solver, posted, send, engine, parallel };
}

describe('The page of the solver', function () {
    it('should be ready, and solve the document the extension sends, as it changes', function () {
        const { solver, posted, send, engine, parallel } = page();
        deepEqual(posted, [{ type: 'ready' }]);
        equal(solver.engine, engine);
        send({ type: 'document', name: 'tweety.arg2p', theory: 'f :=> a.', query: 'a', flags: 'queryMode.', parallel: true });
        deepEqual(
            { fileName: solver.fileName, theory: solver.theory, query: solver.query, flags: solver.flags, parallel: parallel.checked },
            { fileName: 'tweety', theory: 'f :=> a.', query: 'a', flags: 'queryMode.', parallel: true },
        );
        send({ type: 'theory', theory: 'f :=> b.' });
        equal(solver.theory, 'f :=> b.');
    });

    it('should tell the extension the theory drawn, the query, flags and switch Parallel changed, and where the theory does not parse', function () {
        const { solver, posted, parallel } = page();
        solver.tell('arg2p-theory-change', { theory: 'a.' });
        solver.tell('arg2p-query-change', { query: 'b' });
        solver.tell('arg2p-flags-change', { flags: 'queryMode.' });
        const problem = { line: 2, column: 3, length: 1, message: 'unexpected' };
        solver.tell('arg2p-theory-error', { problem });
        parallel.checked = true;
        parallel.dispatchEvent(new Event('change'));
        deepEqual(posted.slice(1), [
            { type: 'edit', theory: 'a.' },
            { type: 'state', query: 'b', flags: '', parallel: false },
            { type: 'state', query: 'b', flags: 'queryMode.', parallel: false },
            { type: 'problem', problem },
            { type: 'state', query: 'b', flags: 'queryMode.', parallel: true },
        ]);
    });

    it('should solve on the extension, and stop what it waits for once the server stopped', async function () {
        const { posted, send, engine } = page();
        const solving = engine.solve({ theory: 'f :=> a.', query: 'a', flags: '' });
        const asked = posted.flatMap(it => (it.type === 'engine' ? [it.request] : []));
        deepEqual(
            asked.map(it => it.type),
            ['warmUp', 'solve'],
        );
        const id = (asked[1] as { id: number }).id;
        send({ type: 'engine', answer: { type: 'started', id, query: 'a' } });
        equal((await solving).query, 'a');
        const next = engine.check('f :=> a.', 'theory');
        send({ type: 'stopped' });
        const error = await next.then(
            () => null,
            (it: EngineError) => it,
        );
        equal(error?.message, STOPPED);
        // the server stops the resolution of the page, and a fresh one warms up
        deepEqual(
            posted.slice(-2).map(it => (it.type === 'engine' ? it.request.type : it.type)),
            ['stop', 'warmUp'],
        );
    });
});
