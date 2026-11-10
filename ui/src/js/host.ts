import { Arg2p, Flags, Value, type Solutions } from '@tuprolog/arg2p';
import { EMPTY_GRAPH, type EngineError, type GraphData, type SolutionData, type SyntaxProblem } from '../engine.js';
import { FLAGS, type Settings } from '../flags/schema.js';
import { unquote } from '../formats/atoms.js';
import { compactGraphData } from './graph.js';

// The solver's side of the engine of the browser (see JsEngine): it answers messages, posting its answers with `post`.
// Kept apart from the worker, so that it runs (and is tested) anywhere.
//
//   { type: 'warmUp' }                                -> { type: 'warm' }
//   { type: 'solve', id, theory, query, flags }       -> { type: 'started', id, query } or { type: 'failed', id, error },
//                                                        and { type: 'output', id, text } as the solver prints
//   { type: 'next', id }                              -> { type: 'solution', id, solution, hasNext }
//   { type: 'graphAt', id, branch, reqId }            -> { type: 'graph', reqId, graph }
//   { type: 'check', reqId, text, kind }              -> { type: 'checked', reqId, problem }
//   { type: 'settings', reqId, theory, flags }        -> { type: 'settings', reqId, settings }
//
// Only the latest query is kept: a new one replaces it.

export type Request =
    | { type: 'warmUp' }
    | { type: 'solve'; id: number; theory: string; query: string; flags: string }
    | { type: 'next'; id: number }
    | { type: 'graphAt'; id: number; branch: number; reqId: number }
    | { type: 'check'; reqId: number; text: string; kind: 'theory' | 'query' }
    | { type: 'settings'; reqId: number; theory: string; flags: string };

export type Answer =
    | { type: 'warm' }
    | { type: 'started'; id: number; query: string }
    | { type: 'failed'; id: number; error: EngineError }
    | { type: 'output'; id: number; text: string }
    | { type: 'solution'; id: number; solution: SolutionData; hasNext: boolean }
    | { type: 'graph'; reqId: number; graph: GraphData }
    | { type: 'checked'; reqId: number; problem: SyntaxProblem | null }
    | { type: 'settings'; reqId: number; settings: Settings };

export function createHost(post: (answer: Answer) => void): { handle(request: Request): void } {
    let current: { id: number; solver: Arg2p; solutions: Solutions; profile: string } | null = null;

    function handle(request: Request): void {
        switch (request.type) {
            case 'warmUp': return warmUp();
            case 'solve': return solve(request);
            case 'next': return next(request.id);
            case 'graphAt': return graphAt(request);
            case 'check': return post({ type: 'checked', reqId: request.reqId, problem: check(request.text, request.kind) });
            case 'settings': return post({ type: 'settings', reqId: request.reqId, settings: settings(request.theory, request.flags) });
        }
    }

    // The first evaluation is much slower than the next ones (code compiled and initialised on first use): done
    // unseen, in the background
    function warmUp(): void {
        try {
            Arg2p.warmUp();
        } catch {
            // only a warm-up
        }
        post({ type: 'warm' });
    }

    function solve({ id, theory, query, flags }: { id: number; theory: string; query: string; flags: string }): void {
        const written = normalizedQuery(query);
        if (written === '') {
            current = null;
            return post({ type: 'failed', id, error: { name: 'Query error', message: 'Query is a mandatory field.', syntax: null } });
        }
        try {
            // the flags and the theory are both read as the solver is built: the error tells which one it is about
            let parsedFlags: Flags;
            try {
                parsedFlags = Flags.parse(flags);
            } catch (error) {
                throw failure('Flags error', error, 'flags');
            }
            let solver: Arg2p;
            try {
                solver = Arg2p.builder().theory(theory).flags(parsedFlags).output(text => post({ type: 'output', id, text })).build();
            } catch (error) {
                throw failure('Theory error', error, 'theory');
            }
            const profile = profileOf(solver);
            let solutions: Solutions;
            try {
                solutions = solver.solve(written);
            } catch (error) {
                throw failure('Query error', error, 'query');
            }
            current = { id, solver, solutions, profile };
            post({ type: 'started', id, query: written });
        } catch (error) {
            current = null;
            post({ type: 'failed', id, error: error as EngineError });
        }
    }

    // A solution, and whether there is another one (known only by looking for it)
    function next(id: number): void {
        if (current === null || current.id !== id) {
            return post({ type: 'failed', id, error: { name: 'Error', message: 'This query is no longer running.', syntax: null } });
        }
        let solution: SolutionData;
        let hasNext = false;
        try {
            const found = current.solutions.next();
            const { graph, branch } = currentGraph(current.solver, current.profile);
            solution = {
                result: found.isYes ? 'yes' : found.isNo ? 'no' : 'halt',
                query: found.query,
                bindings: found.variables.map(name => ({ name, value: found.text(name) ?? '' })),
                error: found.error ?? null,
                graph,
                branch,
            };
            hasNext = current.solutions.hasNext();
        } catch (error) {
            solution = { result: 'halt', query: '', bindings: [], error: messageOf(error), graph: EMPTY_GRAPH, branch: 0 };
        }
        post({ type: 'solution', id, solution, hasNext });
    }

    function graphAt({ id, branch, reqId }: { id: number; branch: number; reqId: number }): void {
        let graph = EMPTY_GRAPH;
        try {
            if (current !== null && current.id === id) graph = compactGraphData(current.solver.compactGraphAt(branch), current.profile);
        } catch {
            // a stage that cannot be read shows as empty
        }
        post({ type: 'graph', reqId, graph });
    }

    return { handle };
}

// The graph of the last stage of the last resolution: none without one (a plain Prolog goal builds no graph)
function currentGraph(solver: Arg2p, profile: string): { graph: GraphData; branch: number } {
    try {
        const branch = solver.activeBranch;
        return { graph: compactGraphData(solver.compactGraphAt(branch), profile), branch };
    } catch {
        return { graph: EMPTY_GRAPH, branch: 0 };
    }
}

// The profile in effect for the theory of [solver] (the theory states it, or the flags, or it is the default), asked
// before the query, as the setting does not change the context of the solver (until the API gives it)
function profileOf(solver: Arg2p): string {
    try {
        const found = solver.solve('setting(graphBuildMode, Profile)').next();
        return found.isYes ? found.text('Profile') ?? '' : '';
    } catch {
        return '';
    }
}

// The query [text] as the solver is given it: with no blanks around it, nor a full stop at its end (empty for none)
function normalizedQuery(text: string): string {
    return text.trim().replace(/\.$/, '');
}

/** Where [text], a theory (or flags) or a query, does not parse; null if it does */
export function check(text: string, kind: 'theory' | 'query'): SyntaxProblem | null {
    const query = kind === 'query' ? normalizedQuery(text) : null;
    if (query === '') return null;
    try {
        // a theory read as a solver reads it (until the API checks texts by itself)
        if (query !== null) Value.parse(query);
        else Arg2p.of(text, Flags.none());
        return null;
    } catch (error) {
        return syntaxProblem(error) ?? { line: 1, column: 1, length: 1, message: messageOf(error) };
    }
}

/**
 * The values of the flags in effect for [theory] solved with [flags], as the solver checks them (setting/1 for a
 * switch, setting/2 for the values of a flag): the flags alone when the theory does not parse, none when they do not
 * (until the API gives them)
 */
export function settings(theory: string, flags: string): Settings {
    let parsed: Flags;
    try {
        parsed = Flags.parse(flags);
    } catch {
        return {};
    }
    let solver: Arg2p;
    try {
        solver = Arg2p.of(theory, parsed);
    } catch {
        solver = Arg2p.of('', parsed);
    }
    const values: Record<string, string[]> = {};
    FLAGS.forEach(flag => {
        try {
            if (flag.kind === 'switch') {
                values[flag.name] = solver.solve(`setting(${flag.name})`).next().isYes ? ['true'] : [];
            } else {
                const found: string[] = [];
                const solutions = solver.solve(`setting(${flag.name}, Value)`);
                while (solutions.hasNext()) {
                    const solution = solutions.next();
                    if (!solution.isYes) break;
                    found.push(unquote(solution.text('Value') ?? ''));
                }
                values[flag.name] = found;
            }
        } catch {
            // a flag this version of arg2p does not know
        }
    });
    values.graphBuildMode = [profileOf(solver)].filter(it => it !== '');
    return values;
}

// An error of the API as an EngineError, where in the text it is when the text does not parse
function failure(name: string, error: unknown, where: 'theory' | 'flags' | 'query'): EngineError {
    const problem = syntaxProblem(error);
    return { name, message: messageOf(error), syntax: problem && { ...problem, where } };
}

// A parse error tells where it is in its message, `... at 3:12; expected ...`: the line and column (from 1) and the
// token it is about (until the API gives them apart)
function syntaxProblem(error: unknown): SyntaxProblem | null {
    const message = messageOf(error);
    const at = / at (\d+):(\d+)/.exec(message);
    if (at === null) return null;
    const token = /Unexpected token '([^']*)'/.exec(message);
    return {
        line: Math.max(1, Number(at[1])),
        column: Math.max(1, Number(at[2])),
        length: Math.max(1, token?.[1].length ?? 1),
        message: message.replace(/ at \d+:\d+/, ''),
    };
}

function messageOf(error: unknown): string {
    if (error instanceof Error) return error.message;
    if (typeof error === 'object' && error !== null && 'message' in error) return String(error.message);
    return String(error);
}
