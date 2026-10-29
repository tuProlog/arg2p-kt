import * as vscode from 'vscode';
import { Federation, type FederationSettings } from './federation.js';
import { page } from './page.js';
import type { FromFederation, ToFederation } from './protocol.js';
import type { LanguageServer } from './server.js';

// The view Arg2P Federation, in the explorer: the federation of the language server (see federation.ts), shown as the
// server tells it, its settings changed as the user asks.

export class FederationView implements vscode.WebviewViewProvider {
    /** The id of the view */
    static readonly ID = 'arg2p.federation';

    private view: vscode.WebviewView | null = null;
    private readonly federation: Federation;

    constructor(
        private readonly context: vscode.ExtensionContext,
        server: LanguageServer,
    ) {
        this.federation = new Federation(server, message => this.send(message), (setting, value) => {
            void vscode.workspace.getConfiguration('arg2p.federation').update(setting, value, vscode.ConfigurationTarget.Global);
        });
        context.subscriptions.push(
            server.onFederationChanged(state => this.federation.changed(state)),
            server.onDidStart(() => void this.federation.refresh()),
            server.onDidStop(() => this.federation.changed(null)),
            vscode.workspace.onDidChangeConfiguration(event => {
                if (event.affectsConfiguration('arg2p.federation')) void this.federation.apply(FederationView.settings());
            }),
        );
    }

    /** The settings of the federation, as the user set them */
    static settings(): FederationSettings {
        const settings = vscode.workspace.getConfiguration('arg2p.federation');
        return { enabled: settings.get<boolean>('enabled', true), name: settings.get<string>('name', '') };
    }

    resolveWebviewView(view: vscode.WebviewView): void {
        const media = vscode.Uri.joinPath(this.context.extensionUri, 'dist');
        view.webview.options = { enableScripts: true, localResourceRoots: [media] };
        view.webview.html = page(view.webview, media, 'federation.js', '<div id="federation"></div>');
        const listening = view.webview.onDidReceiveMessage((message: FromFederation) => void this.federation.handle(message));
        view.onDidDispose(() => {
            listening.dispose();
            this.view = null;
        });
        this.view = view;
    }

    private send(message: ToFederation): void {
        void this.view?.webview.postMessage(message);
    }
}
