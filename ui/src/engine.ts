import type { Settings } from './flags/schema.js';

// The engine the components run on: the solver of the browser (see ./js), or one of the JVM (a language server, for
// the extension of VS Code). Everything it gives is plain data, as it crosses a worker or a process.

/** An argument of a graph, by its identifier (A0, A1, ...) */
export interface ArgumentData {
    readonly id: string;
    /** How it is written, `A2 : A0,r1 : c` */
    readonly descriptor: string;
    /** The statement it concludes, as arg2p writes it (`-a`) */
    readonly conclusion: string;
    /** Its top rule, `none` for a premise */
    readonly topRule: string;
    /** in, out, und, or na for an argument the labelling does not label */
    readonly label: string;
    /** The identifiers of the arguments it is built on */
    readonly supports: readonly string[];
}

/** An attack, by the identifiers of its arguments: its type (rebut, undermine, undercut, ...) and the sub-argument hit */
export interface AttackData {
    readonly from: string;
    readonly to: string;
    readonly type: string | null;
    readonly on: string | null;
}

export interface GraphData {
    /**
     * The profile the theory was read in (aspic_plus, aba, abstract, ...: the value of graphBuildMode in effect), empty
     * when unknown. In an abstract framework the arguments are named by the theory (their conclusion), with no structure.
     */
    readonly profile: string;
    readonly arguments: readonly ArgumentData[];
    readonly attacks: readonly AttackData[];
}

export const EMPTY_GRAPH: GraphData = { profile: '', arguments: [], attacks: [] };

/** A solution of a query: yes, no or halt (an error), what it binds, and the graph it computed */
export interface SolutionData {
    readonly result: 'yes' | 'no' | 'halt';
    /** The query, its variables bound for a yes solution */
    readonly query: string;
    readonly bindings: readonly { readonly name: string; readonly value: string }[];
    /** The error of a halt solution */
    readonly error: string | null;
    readonly graph: GraphData;
    /** The context branch the graph is in: the stages 0 to branch can be read back (see Run.graphAt) */
    readonly branch: number;
}

/** Where a text does not parse: its line and column (from 1), the length of the token, and why */
export interface SyntaxProblem {
    readonly line: number;
    readonly column: number;
    readonly length: number;
    readonly message: string;
}

/** An evaluation that failed: a theory, flags or a query that cannot be read, a theory its profile refuses, ... */
export interface EngineError {
    readonly name: string;
    readonly message: string;
    /** Where the text does not parse, and which text it is */
    readonly syntax: (SyntaxProblem & { readonly where: 'theory' | 'flags' | 'query' }) | null;
    /** The evaluation was stopped (see Engine.stop) */
    readonly stopped?: boolean;
}

/** The solutions of a query, computed one at a time as they are asked for */
export interface Run {
    /** The query, as the engine read it */
    readonly query: string;
    hasNext(): boolean;
    next(): Promise<SolutionData>;
    /** The graph of the stage [branch] of the computation: none once the run is replaced, or stopped (it fails) */
    graphAt(branch: number): Promise<GraphData>;
}

export interface SolveRequest {
    readonly theory: string;
    readonly query: string;
    /** The flags, as Prolog facts: the solver has these alone */
    readonly flags: string;
}

export interface Engine {
    /** The solutions of [request] (a rejected promise holds an EngineError); what the solver prints goes to [output] */
    solve(request: SolveRequest, output?: (text: string) => void): Promise<Run>;
    /** Where [text], a theory (or flags) or a query, does not parse; null if it does */
    check(text: string, kind: 'theory' | 'query'): Promise<SyntaxProblem | null>;
    /**
     * The values of the flags in effect for [theory] solved with [flags]: those the theory states, or else those of
     * the flags that differ from their default, or else those its profile presets (those of the flags alone when the
     * theory does not parse)
     */
    settings(theory: string, flags: string): Promise<Settings>;
    /**
     * Stops the evaluation going on: what its query was waited for fails, as stopped; the checks and the settings
     * asked meanwhile are answered still
     */
    stop(): void;
}

/** A solution, and the run it comes from: the stages of its computation are read back through it */
export interface Solved {
    readonly solution: SolutionData;
    readonly run: Pick<Run, 'graphAt'> | null;
}
