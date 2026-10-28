import { EMPTY_GRAPH, type EngineError } from '@tuprolog/arg2p-ui/data';
import type { Answer } from '@tuprolog/arg2p-ui/js';
import type { EngineRequest } from './protocol.js';

// The engine of the solver page, on the language server: the page speaks to its engine as the browser does to the
// worker of its solver (the messages of @tuprolog/arg2p-ui/js), and the bridge passes them on to the server
// (arg2p/engine), in a session of the page's own, so that the runs of a page never mix with those of another.

/** The language server, as the bridge asks it */
export interface Server {
    request<T>(method: string, params: unknown): Promise<T>;
}

export class Bridge {
    private session = sessionId();
    private disposed = false;

    constructor(
        private readonly server: Server,
        private readonly post: (answer: Answer) => void,
    ) {}

    /** Answers [request] of the page, through the server */
    async handle(request: EngineRequest): Promise<void> {
        const session = this.session;
        const answers = await this.server.request<{ answers: Answer[] }>('arg2p/engine', { session, request }).then(
            it => it.answers,
            error => unanswered(request, error),
        );
        // a run the server started as the page closed: forgotten as well
        if (this.disposed) return this.close(session);
        // the answers of a session the server stopped with are no longer the page's
        if (session === this.session) answers.forEach(it => this.post(it));
    }

    /** Starts a session afresh: the server ran the last one, and stopped */
    renew(): void {
        this.session = sessionId();
    }

    /** Forgets the session of the page, on the server too, stopping what runs in it */
    dispose(): void {
        this.disposed = true;
        this.close(this.session);
    }

    private close(session: string): void {
        for (const request of [{ type: 'stop' }, { type: 'close' }]) {
            this.server.request('arg2p/engine', { session, request }).catch(() => undefined);
        }
    }
}

function sessionId(): string {
    return crypto.randomUUID();
}

// The answers of [request] the server did not give: [error], or nothing
function unanswered(request: EngineRequest, error: unknown): Answer[] {
    switch (request.type) {
        case 'warmUp':
            return [{ type: 'warm' }];
        case 'solve':
        case 'next':
            return [{ type: 'failed', id: request.id, error: failure(error) }];
        case 'graphAt':
            return [{ type: 'graph', reqId: request.reqId, graph: EMPTY_GRAPH }];
        case 'check':
            return [{ type: 'checked', reqId: request.reqId, problem: null }];
        case 'settings':
            return [{ type: 'settings', reqId: request.reqId, settings: {} }];
        case 'stop':
            return [];
    }
}

function failure(error: unknown): EngineError {
    const message = error instanceof Error ? error.message : String(error);
    return { name: 'Solver error', message, syntax: null };
}
