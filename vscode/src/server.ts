import { Emitter, type Event } from 'vscode-jsonrpc';
import type { LanguageClient } from 'vscode-languageclient/node';
import type { Server } from './bridge.js';
import type { FederationState } from './protocol.js';

// The language server of arg2p, as the extension runs it: started, stopped, and started afresh (when the java of the
// settings changes, say). What ran on a server is gone with it: the sessions of the solver panels, see onDidStop. It
// tells how its federation changes (arg2p/federationChanged).

/** The client of a language server, as vscode-languageclient makes it */
export type Client = Pick<LanguageClient, 'start' | 'stop' | 'dispose' | 'isRunning' | 'sendRequest' | 'onDidChangeState' | 'onNotification'>;

export class LanguageServer implements Server {
    private client: Client | null = null;
    // the starts and the stops, one after the other: a client is stopped once its start is over
    private operations: Promise<void> = Promise.resolve();
    private readonly stopped = new Emitter<void>();
    private readonly started = new Emitter<void>();
    private readonly federation = new Emitter<FederationState>();

    /** The server that ran stopped: it was stopped, restarted or it crashed (and the client restarts it) */
    readonly onDidStop: Event<void> = this.stopped.event;

    /** A server started: started at first, restarted, or restarted by the client after it crashed */
    readonly onDidStart: Event<void> = this.started.event;

    /** The federation of the server changed: its state now */
    readonly onFederationChanged: Event<FederationState> = this.federation.event;

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

    // A client started, its errors reported: the server is told started and stopped as it comes to run and stops running,
    // its first start as its restarts by the client after a crash
    private async launch(): Promise<void> {
        let client: Client;
        try {
            client = this.spawn();
        } catch (error) {
            this.report(error);
            return;
        }
        this.client = client;
        let running = false;
        client.onDidChangeState(() => {
            const now = client.isRunning();
            if (now !== running) (now ? this.started : this.stopped).fire();
            running = now;
        });
        client.onNotification('arg2p/federationChanged', (state: FederationState) => this.federation.fire(state));
        await client.start().catch(error => this.report(error));
    }

    // A client running is stopped, and disposed of if it does not stop (in time, as when its server hangs); one that is not
    // running (its start failed) cannot be stopped: it is disposed of. Either way it is gone, and the next one can start.
    private async halt(): Promise<void> {
        const client = this.client;
        this.client = null;
        if (client === null) return;
        if (client.isRunning()) {
            const stopped = await client.stop().then(
                () => true,
                () => false,
            );
            if (stopped) return;
        }
        await client.dispose().catch(() => undefined);
    }
}
