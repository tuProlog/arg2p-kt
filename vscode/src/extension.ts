import * as vscode from 'vscode';
import { LanguageClient, type LanguageClientOptions, type ServerOptions } from 'vscode-languageclient/node';
import { argumentsOf, javaOf } from './java.js';
import { SolverPanel } from './panel.js';

// The extension of Arg2P: the language arg2p (*.arg2p), its errors as it is typed, and the solver of a theory, both
// from the language server of arg2p, which runs on the JVM

let client: LanguageClient | null = null;

export async function activate(context: vscode.ExtensionContext): Promise<void> {
    context.subscriptions.push(
        vscode.commands.registerCommand('arg2p.solve', () => {
            const editor = vscode.window.activeTextEditor;
            if (editor === undefined || editor.document.languageId !== 'arg2p') {
                void vscode.window.showInformationMessage('Open a theory of Arg2P (a .arg2p file) to solve it.');
                return;
            }
            void SolverPanel.show(context, server, editor);
        }),
        vscode.commands.registerCommand('arg2p.restartServer', async () => {
            await client?.restart();
        }),
        vscode.workspace.onDidChangeConfiguration(async event => {
            if (event.affectsConfiguration('arg2p.java')) {
                await stop();
                await start(context);
            }
        }),
    );
    await start(context);
}

export async function deactivate(): Promise<void> {
    await stop();
}

async function start(context: vscode.ExtensionContext): Promise<void> {
    const settings = vscode.workspace.getConfiguration('arg2p');
    const java = javaOf(settings.get<string>('java.path') ?? '', context.extensionPath, process.env.JAVA_HOME);
    const jar = context.asAbsolutePath('server/arg2p-lsp.jar');
    const serverOptions: ServerOptions = { command: java, args: argumentsOf(settings.get<string[]>('java.options') ?? [], jar) };
    const clientOptions: LanguageClientOptions = { documentSelector: [{ language: 'arg2p' }] };
    client = new LanguageClient('arg2p', 'Arg2P', serverOptions, clientOptions);
    client.onNotification('arg2p/output', (params: { id: number; text: string }) => SolverPanel.output(params));
    try {
        await client.start();
    } catch (error) {
        void vscode.window.showErrorMessage(
            `The language server of Arg2P did not start (${java}): ${error instanceof Error ? error.message : String(error)}. ` +
                'Set arg2p.java.path to a Java 21 or later.',
        );
    }
}

async function stop(): Promise<void> {
    const running = client;
    client = null;
    await running?.stop();
}

// The language server, as the solver panels ask it: the one running now (restarted, it is another)
function server() {
    return {
        request: <T>(method: string, params: unknown): Promise<T> =>
            client === null ? Promise.reject(new Error('The language server of Arg2P is not running.')) : client.sendRequest<T>(method, params),
        notify: (method: string, params: unknown): void => {
            void client?.sendNotification(method, params);
        },
    };
}
