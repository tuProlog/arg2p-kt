import type { Answer, Request } from '@tuprolog/arg2p-ui/js';
import type { EngineError, GraphData, SolutionData, SyntaxProblem } from '@tuprolog/arg2p-ui/data';

// The engine of the solver page, on the language server: the page speaks to its engine as the browser does to the
// worker of its solver (the messages of @tuprolog/arg2p-ui/js), and the bridge turns them into the requests of arg2p
// to the server (arg2p/solve, arg2p/next, ...), and their answers back into messages.

/** The language server, as the bridge asks it */
export interface Server {
    request<T>(method: string, params: unknown): Promise<T>;
    notify(method: string, params: unknown): void;
}

interface SolveAnswer {
    id: number;
    query?: string;
    error?: EngineError;
}

interface NextAnswer {
    solution?: SolutionData;
    hasNext: boolean;
    error?: EngineError;
}

const EMPTY_GRAPH: GraphData = { profile: '', arguments: [], attacks: [] };

export class Bridge {
    // the runs of the page by the ids it gives them, as the server knows them, and the other way round
    private readonly runs = new Map<number, number>();
    private readonly pages = new Map<number, number>();

    constructor(
        private readonly server: Server,
        private readonly post: (answer: Answer) => void,
    ) {}

    /** Answers [request] of the page, through the server */
    async handle(request: Request): Promise<void> {
        switch (request.type) {
            case 'warmUp':
                return this.post({ type: 'warm' });
            case 'solve': {
                const { id, theory, query, flags } = request;
                try {
                    const answer = await this.server.request<SolveAnswer>('arg2p/solve', { theory, query, flags });
                    // the page shows its last query alone: the server forgets the others
                    this.dispose();
                    this.runs.set(id, answer.id);
                    this.pages.set(answer.id, id);
                    if (answer.error) this.post({ type: 'failed', id, error: answer.error });
                    else this.post({ type: 'started', id, query: answer.query ?? query });
                } catch (error) {
                    this.post({ type: 'failed', id, error: failure(error) });
                }
                return;
            }
            case 'next': {
                const run = this.runs.get(request.id);
                if (run === undefined) {
                    return this.post({ type: 'failed', id: request.id, error: { name: 'Error', message: 'This query is no longer running.', syntax: null } });
                }
                try {
                    const answer = await this.server.request<NextAnswer>('arg2p/next', { id: run });
                    if (answer.error || !answer.solution) this.post({ type: 'failed', id: request.id, error: answer.error ?? failure('No solution') });
                    else this.post({ type: 'solution', id: request.id, solution: answer.solution, hasNext: answer.hasNext });
                } catch (error) {
                    this.post({ type: 'failed', id: request.id, error: failure(error) });
                }
                return;
            }
            case 'graphAt': {
                const run = this.runs.get(request.id);
                const graph =
                    run === undefined ? EMPTY_GRAPH : await this.server.request<GraphData>('arg2p/graphAt', { id: run, branch: request.branch }).catch(() => EMPTY_GRAPH);
                return this.post({ type: 'graph', reqId: request.reqId, graph });
            }
            case 'check': {
                const problem = await this.server.request<SyntaxProblem | null>('arg2p/check', { text: request.text, kind: request.kind }).catch(() => null);
                return this.post({ type: 'checked', reqId: request.reqId, problem: problem ?? null });
            }
            case 'settings': {
                const settings = await this.server
                    .request<Record<string, string[]>>('arg2p/settings', { theory: request.theory, flags: request.flags })
                    .catch(() => ({}));
                return this.post({ type: 'settings', reqId: request.reqId, settings });
            }
        }
    }

    /** What the run [id] of the server prints, for the page that started it */
    output(params: { id: number; text: string }): void {
        const page = this.pages.get(params.id);
        if (page !== undefined) this.post({ type: 'output', id: page, text: params.text });
    }

    /** Forgets every run of the page, on the server too */
    dispose(): void {
        [...this.runs.keys()].forEach(id => this.forget(id));
    }

    private forget(page: number): void {
        const run = this.runs.get(page);
        if (run === undefined) return;
        this.runs.delete(page);
        this.pages.delete(run);
        this.server.notify('arg2p/close', { id: run });
    }
}

function failure(error: unknown): EngineError {
    const message = error instanceof Error ? error.message : String(error);
    return { name: 'Solver error', message, syntax: null };
}
