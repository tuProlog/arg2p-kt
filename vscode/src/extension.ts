import * as vscode from 'vscode';
import { LanguageClient } from 'vscode-languageclient/node';
import { argumentsOf, javaOf } from './java.js';
import { SolverPanel } from './panel.js';
import { LanguageServer } from './server.js';

// The extension of Arg2P: the language arg2p (*.arg2p), its errors as it is typed, and the solver of a theory, both
// from the language server of arg2p, which runs on the JVM

let server: LanguageServer | null = null;

export async function activate(context: vscode.ExtensionContext): Promise<void> {
    const languageServer = new LanguageServer(
        () => {
            const { java, args } = command(context);
            return new LanguageClient('arg2p', 'Arg2P', { command: java, args }, { documentSelector: [{ language: 'arg2p' }] });
        },
        error =>
            void vscode.window.showErrorMessage(
                `The language server of Arg2P did not start (${command(context).java}): ${error instanceof Error ? error.message : String(error)}. ` +
                    'Set arg2p.java.path to a Java 21 or later.',
            ),
    );
    server = languageServer;
    context.subscriptions.push(
        vscode.commands.registerCommand('arg2p.solve', () => {
            const editor = vscode.window.activeTextEditor;
            if (editor === undefined || editor.document.languageId !== 'arg2p') {
                void vscode.window.showInformationMessage('Open a theory of Arg2P (a .arg2p file) to solve it.');
                return;
            }
            void SolverPanel.show(context, languageServer, editor);
        }),
        vscode.commands.registerCommand('arg2p.restartServer', () => languageServer.restart()),
        vscode.workspace.onDidChangeConfiguration(async event => {
            if (event.affectsConfiguration('arg2p.java')) {
                await languageServer.restart();
                changed.fire();
            }
        }),
        changed,
        // the MCP server of arg2p, for the AI assistants of VS Code: the same jar, on the same java, with --mcp
        vscode.lm.registerMcpServerDefinitionProvider('arg2p', {
            onDidChangeMcpServerDefinitions: changed.event,
            provideMcpServerDefinitions: () => {
                const { java, args } = command(context);
                return [new vscode.McpStdioServerDefinition('Arg2P', java, [...args, '--mcp'], {}, context.extension.packageJSON.version)];
            },
        }),
    );
    await languageServer.start();
}

// The definitions of the MCP server change with the java running it
const changed = new vscode.EventEmitter<void>();

// java, and its arguments running the server: the language server, or with --mcp the MCP server
function command(context: vscode.ExtensionContext): { java: string; args: string[] } {
    const settings = vscode.workspace.getConfiguration('arg2p');
    const java = javaOf(settings.get<string>('java.path') ?? '', context.extensionPath, process.env.JAVA_HOME);
    return { java, args: argumentsOf(settings.get<string[]>('java.options') ?? [], context.asAbsolutePath('server/arg2p-lsp.jar')) };
}

export async function deactivate(): Promise<void> {
    await server?.stop();
}
