import type { SyntaxProblem } from '@tuprolog/arg2p-ui/data';
import { JsEngine, type Channel } from '@tuprolog/arg2p-ui/js';
import type { Arg2pSolver } from '@tuprolog/arg2p-ui/solver';
import type { FromPage, ToPage } from '../src/protocol.js';

// The solver of the page, bound to the extension: its engine is the language server (the extension passes the messages
// of the engine on, see bridge.ts), its theory the document the panel belongs to. The extension sends the theory as it
// changes, and is told what the solver changes: the theory drawn, which it writes into the document, the query and the
// flags, which it remembers for the document, and where the theory does not parse, which it shows in the document.
// The switch Parallel tells whether the queries are solved on the federation of the server, remembered with them.

/** The extension, as the page speaks to it */
export interface VsCode {
    postMessage(message: FromPage): void;
}

/** The solver, as the extension fills it and is told of it */
export type Solver = Pick<Arg2pSolver, 'engine' | 'theory' | 'query' | 'flags' | 'fileName'> & EventTarget;

/** A switch, as the extension sets it and is told of it */
export type Switch = { checked: boolean } & EventTarget;

/**
 * Binds [solver] to [vscode], whose messages come to [page]: the engine of the solver, on the language server, solving
 * in parallel as [parallel] says
 */
export function connect(solver: Solver, vscode: VsCode, page: EventTarget, parallel: Switch): JsEngine {
    // The channel of the engine: to the extension, and back from it
    let channel: Channel | null = null;
    const engine = new JsEngine(() => {
        const opened: Channel = {
            onmessage: null,
            postMessage: request => vscode.postMessage({ type: 'engine', request }),
            // the resolution of the page is stopped on the server, which goes on with the others
            terminate: () => vscode.postMessage({ type: 'engine', request: { type: 'stop' } }),
        };
        channel = opened;
        return opened;
    });
    solver.engine = engine;

    page.addEventListener('message', event => {
        const message = (event as MessageEvent<ToPage>).data;
        if (message.type === 'engine') channel?.onmessage?.({ data: message.answer });
        // the server stopped, and what the page waited for with it
        else if (message.type === 'stopped') engine.stop();
        else if (message.type === 'theory') solver.theory = message.theory;
        else if (message.type === 'document') {
            solver.fileName = message.name.replace(/\.arg2p$/, '');
            solver.theory = message.theory;
            solver.query = message.query;
            solver.flags = message.flags;
            parallel.checked = message.parallel;
        }
    });

    const on = <T>(type: string, action: (detail: T) => void) => solver.addEventListener(type, event => action((event as CustomEvent<T>).detail));
    const remember = () => vscode.postMessage({ type: 'state', query: solver.query, flags: solver.flags, parallel: parallel.checked });
    on('arg2p-query-change', remember);
    on('arg2p-flags-change', remember);
    parallel.addEventListener('change', remember);
    on<{ theory: string }>('arg2p-theory-change', ({ theory }) => vscode.postMessage({ type: 'edit', theory }));
    on<{ problem: SyntaxProblem }>('arg2p-theory-error', ({ problem }) => vscode.postMessage({ type: 'problem', problem }));

    vscode.postMessage({ type: 'ready' });
    return engine;
}
