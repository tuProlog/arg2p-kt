import type { SyntaxProblem } from '@tuprolog/arg2p-ui/data';
import type { Answer, Request } from '@tuprolog/arg2p-ui/js';

// The messages between the solver panel (panel.ts) and its page (webview/main.ts), in the bundles of both

/** What the page asks its engine, the language server (see bridge.ts): a request of the solver, or to stop the running one */
export type EngineRequest = Request | { type: 'stop' };

/** What the page tells the extension: `problem` where the theory, the document, does not parse */
export type FromPage =
    | { type: 'ready' }
    | { type: 'engine'; request: EngineRequest }
    | { type: 'edit'; theory: string }
    | { type: 'state'; query: string; flags: string }
    | { type: 'problem'; problem: SyntaxProblem };

/** What the extension tells the page: `stopped` when the server stopped, and what the page waited for with it */
export type ToPage =
    | { type: 'document'; name: string; theory: string; query: string; flags: string }
    | { type: 'theory'; theory: string }
    | { type: 'engine'; answer: Answer }
    | { type: 'stopped' };
