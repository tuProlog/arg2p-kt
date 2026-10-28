import { Emitter, type Event } from 'vscode-jsonrpc';
import type { LanguageClient } from 'vscode-languageclient/node';
import type { Server } from './bridge.js';

// The language server of arg2p, as the extension runs it: started, stopped, and started afresh (when the java of the
// settings changes, say). What ran on a server is gone with it: the sessions of the solver panels, see onDidStop.

/** The client of a language server, as vscode-languageclient makes it */
export type Client = Pick<LanguageClient, 'start' | 'stop' | 'dispose' | 'isRunning' | 'sendRequest' | 'onDidChangeState'>;

export class LanguageServer implements Server {
    private client: Client | null = null;
    // the starts and the stops, one after the other: a client is stopped once its start is over
    private operations: Promise<void> = Promise.resolve();
    private readonly stopped = new Emitter<void>();

    /** The server that ran stopped: it was stopped, restarted or it crashed (and the client restarts it) */
    readonly onDidStop: Event<void> = this.stopped.event;

    constructor(
        // a client of the server, as the settings tell it now
        private readonly spawn: () => Client,
        // what tells the user that the server did not start
        private readonly report: (error: unknown) => void,
    ) {}

    start(): Promise<void> {
        return this.then(() => this.launch());
    }

    stop(): Promise<void> {
        return this.then(() => this.halt());
    }

    restart(): Promise<void> {
        return this.then(async () => {
            await this.halt();
            await this.launch();
        });
    }

    request<T>(method: string, params: unknown): Promise<T> {
        return this.client === null ? Promise.reject(new Error('The language server of Arg2P is not running.')) : this.client.sendRequest<T>(method, params);
    }

    private then(operation: () => Promise<void>): Promise<void> {
        const done = this.operations.then(operation);
        this.operations = done.catch(() => undefined);
        return done;
    }

    private async launch(): Promise<void> {
        const client = this.spawn();
        this.client = client;
        let running = false;
        client.onDidChangeState(() => {
            if (running && !client.isRunning()) this.stopped.fire();
            running = client.isRunning();
        });
        await client.start().catch(error => this.report(error));
    }

    // A client that is not running (its start failed) cannot be stopped: it is disposed of
    private async halt(): Promise<void> {
        const client = this.client;
        this.client = null;
        if (client === null) return;
        if (client.isRunning()) await client.stop();
        else await client.dispose().catch(() => undefined);
    }
}
