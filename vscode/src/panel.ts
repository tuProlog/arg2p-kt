import type { SyntaxProblem } from '@tuprolog/arg2p-ui/data';
import * as vscode from 'vscode';
import { Bridge } from './bridge.js';
import { page } from './page.js';
import type { FromPage, ToPage } from './protocol.js';
import type { LanguageServer } from './server.js';

// The solver of a theory: a panel beside its document, with the query, the solutions, the graph (and the drawing of an
// abstract framework) and the flags, the components of @tuprolog/arg2p-ui on the language server, which solves the
// queries in parallel on its federation if the switch Parallel says so. The theory is the text of the document: the
// panel follows it as it is edited, and the drawing edits it.

interface Saved {
    query: string;
    flags: string;
    parallel?: boolean;
}

export class SolverPanel {
    private static readonly panels = new Map<string, SolverPanel>();

    /**
     * The panel of [document], opened under it (in a group of editors of its own) or beside it, as arg2p.solver.position
     * says; shown, if it is open already. The theory keeps the focus, to go on editing it.
     */
    static async show(context: vscode.ExtensionContext, server: LanguageServer, editor: vscode.TextEditor): Promise<void> {
        const document = editor.document;
        const key = document.uri.toString();
        const open = SolverPanel.panels.get(key);
        if (open !== undefined) return open.panel.reveal(undefined, true);
        const below = vscode.workspace.getConfiguration('arg2p').get<string>('solver.position', 'beside') === 'below';
        // a group under the one of the theory, made active: the panel opens there
        if (below) await vscode.commands.executeCommand('workbench.action.newGroupBelow');
        SolverPanel.panels.set(key, new SolverPanel(context, server, document, below ? vscode.ViewColumn.Active : vscode.ViewColumn.Beside));
        if (below) await vscode.window.showTextDocument(document, { viewColumn: editor.viewColumn, preserveFocus: false });
    }

    private readonly panel: vscode.WebviewPanel;
    private readonly bridge: Bridge;
    private readonly subscriptions: vscode.Disposable[] = [];
    // the text the page drew last: the change it makes to the document is not sent back to it
    private drawn: string | null = null;

    private constructor(
        private readonly context: vscode.ExtensionContext,
        server: LanguageServer,
        private readonly document: vscode.TextDocument,
        column: vscode.ViewColumn,
    ) {
        const media = vscode.Uri.joinPath(context.extensionUri, 'dist');
        this.panel = vscode.window.createWebviewPanel('arg2p.solver', `Solve ${baseName(document.uri)}`, { viewColumn: column, preserveFocus: true }, {
            enableScripts: true,
            retainContextWhenHidden: true,
            localResourceRoots: [media],
        });
        this.panel.iconPath = vscode.Uri.joinPath(context.extensionUri, 'media', 'icon.png');
        this.bridge = new Bridge(server, answer => this.send({ type: 'engine', answer }));
        // the session of the panel went with the server: the page is told that its run stopped, and starts another
        server.onDidStop(
            () => {
                this.bridge.renew();
                this.send({ type: 'stopped' });
            },
            null,
            this.subscriptions,
        );
        this.panel.webview.html = page(this.panel.webview, media, 'webview.js', '<arg2p-solver></arg2p-solver>');
        this.panel.webview.onDidReceiveMessage((message: FromPage) => this.receive(message), null, this.subscriptions);
        vscode.workspace.onDidChangeTextDocument(
            event => {
                if (event.document !== this.document) return;
                const theory = this.document.getText();
                if (theory !== this.drawn) this.send({ type: 'theory', theory });
                this.drawn = null;
            },
            null,
            this.subscriptions,
        );
        vscode.workspace.onDidCloseTextDocument(closed => closed === this.document && this.panel.dispose(), null, this.subscriptions);
        this.panel.onDidDispose(() => this.dispose(), null, this.subscriptions);
    }

    private receive(message: FromPage): void {
        switch (message.type) {
            case 'ready': {
                const saved = this.saved();
                this.bridge.parallel = saved.parallel ?? false;
                return this.send({ type: 'document', name: baseName(this.document.uri), theory: this.document.getText(), ...saved, parallel: this.bridge.parallel });
            }
            case 'engine':
                void this.bridge.handle(message.request);
                return;
            case 'edit':
                void this.edit(message.theory);
                return;
            case 'state':
                this.bridge.parallel = message.parallel;
                void this.context.workspaceState.update(this.key, { query: message.query, flags: message.flags, parallel: message.parallel } satisfies Saved);
                return;
            case 'problem':
                void this.reveal(message.problem);
        }
    }

    // Where the theory does not parse: told, and shown in the document if asked (its problem is there, the language
    // server reports it), the focus left where it is otherwise
    private async reveal(problem: SyntaxProblem): Promise<void> {
        const go = 'Go to the error';
        const asked = await vscode.window.showWarningMessage(`The theory does not parse at line ${problem.line}.`, go);
        if (asked !== go) return;
        const at = new vscode.Position(problem.line - 1, problem.column - 1);
        const shown = vscode.window.visibleTextEditors.find(it => it.document === this.document);
        await vscode.window.showTextDocument(this.document, { viewColumn: shown?.viewColumn, selection: new vscode.Range(at, at.translate(0, problem.length)) });
    }

    // The theory drawn, written into the document
    private async edit(theory: string): Promise<void> {
        if (theory === this.document.getText()) return;
        this.drawn = theory;
        const edit = new vscode.WorkspaceEdit();
        edit.replace(this.document.uri, new vscode.Range(this.document.positionAt(0), this.document.positionAt(this.document.getText().length)), theory);
        await vscode.workspace.applyEdit(edit);
    }

    private saved(): Saved {
        const flags = vscode.workspace.getConfiguration('arg2p').get<string>('flags') ?? '';
        return this.context.workspaceState.get<Saved>(this.key) ?? { query: 'buildLabelSets(In, Out, Und)', flags };
    }

    private get key(): string {
        return `arg2p.solver:${this.document.uri.toString()}`;
    }

    private send(message: ToPage): void {
        void this.panel.webview.postMessage(message);
    }

    private dispose(): void {
        SolverPanel.panels.delete(this.document.uri.toString());
        this.bridge.dispose();
        this.subscriptions.forEach(it => it.dispose());
    }
}

function baseName(uri: vscode.Uri): string {
    return uri.path.split('/').pop() ?? 'theory';
}
