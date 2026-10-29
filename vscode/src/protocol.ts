import type { SyntaxProblem } from '@tuprolog/arg2p-ui/data';
import type { Answer, Request } from '@tuprolog/arg2p-ui/js';

// The messages between the solver panel (panel.ts) and its page (webview/main.ts), and between the federation view
// (federation.ts) and its page (webview/federation.ts), in the bundles of both

/** What the page asks its engine, the language server (see bridge.ts): a request of the solver, or to stop the running one */
export type EngineRequest = Request | { type: 'stop' };

/** What the page tells the extension: `problem` where the theory, the document, does not parse */
export type FromPage =
    | { type: 'ready' }
    | { type: 'engine'; request: EngineRequest }
    | { type: 'edit'; theory: string }
    | { type: 'state'; query: string; flags: string; parallel: boolean }
    | { type: 'problem'; problem: SyntaxProblem };

/**
 * What the extension tells the page: `stopped` when the server stopped, and what the page waited for with it;
 * `parallel` whether the queries are solved on the federation
 */
export type ToPage =
    | { type: 'document'; name: string; theory: string; query: string; flags: string; parallel: boolean }
    | { type: 'theory'; theory: string }
    | { type: 'engine'; answer: Answer }
    | { type: 'stopped' };

/** The federation of the language server, as it tells it (arg2p/federation, see Federation.kt there) */
export interface FederationState {
    readonly enabled: boolean;
    /** The name of this device, as the others see it */
    readonly name: string;
    /** The code the others link to this device with, while enabled */
    readonly code?: string | null;
    /** The page a phone on the same network joins from, if the device has an address there */
    readonly joinUrl?: string | null;
    /** The workers of this device */
    readonly workers: number;
    /** The devices heard on the network, not linked yet */
    readonly nearby: readonly { readonly id: string; readonly name: string; readonly requiresCode: boolean }[];
    /** The devices linked, with their workers (a phone has no name; a device reached through another, no workers) */
    readonly sites: readonly { readonly id: string; readonly name?: string | null; readonly workers?: number | null; readonly via?: string | null }[];
}

/** What the federation view tells the extension */
export type FromFederation =
    | { type: 'ready' }
    | { type: 'pair'; site: string; code: string }
    | { type: 'newCode' }
    | { type: 'rename'; name: string }
    | { type: 'enable'; enabled: boolean };

/** What the extension tells the federation view: its state (none while the server does not run), or why a request failed */
export type ToFederation = { type: 'state'; state: FederationState | null } | { type: 'failed'; site: string | null; message: string };
