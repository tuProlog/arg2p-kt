import type { Server } from './bridge.js';
import type { FederationState, FromFederation, ToFederation } from './protocol.js';

// The federation of the language server, as its view shows it (see federationView.ts): every VS Code is a device of
// the local network, which the others see with its name, linked to one once its code is typed. The view asks the
// server (arg2p/federation/...), and is told its state as it changes; its name and whether it is enabled are settings
// (arg2p.federation.name, arg2p.federation.enabled), told to the server as they change.

/** The settings of the federation */
export interface FederationSettings {
    readonly enabled: boolean;
    readonly name: string;
}

export class Federation {
    constructor(
        private readonly server: Server,
        // what the view is told
        private readonly post: (message: ToFederation) => void,
        // what changes a setting of the federation
        private readonly configure: (setting: keyof FederationSettings, value: boolean | string) => void,
    ) {}

    /** Answers [message] of the view */
    async handle(message: FromFederation): Promise<void> {
        switch (message.type) {
            case 'ready':
                return this.refresh();
            case 'pair':
                return this.ask('arg2p/federation/pair', { site: message.site, code: message.code }, message.site);
            case 'newCode':
                return this.ask('arg2p/federation/newCode', undefined);
            case 'rename':
                return this.configure('name', message.name);
            case 'enable':
                return this.configure('enabled', message.enabled);
        }
    }

    /** Tells the view the state of the federation, as the server tells it: none if it does not run */
    async refresh(): Promise<void> {
        const state = await this.server.request<FederationState>('arg2p/federation', undefined).catch(() => null);
        this.post({ type: 'state', state });
    }

    /** Tells the view [state], the server told it changed */
    changed(state: FederationState | null): void {
        this.post({ type: 'state', state });
    }

    /** Tells the server [settings], changed */
    async apply(settings: FederationSettings): Promise<void> {
        await this.ask('arg2p/federation/rename', { name: settings.name });
        await this.ask('arg2p/federation/enable', { enabled: settings.enabled });
    }

    // [method] asked of the server: the view told its state, or why it failed (for [site], if it is about one)
    private async ask(method: string, params: unknown, site: string | null = null): Promise<void> {
        try {
            this.changed(await this.server.request<FederationState>(method, params));
        } catch (error) {
            this.post({ type: 'failed', site, message: error instanceof Error ? error.message : String(error) });
        }
    }
}
