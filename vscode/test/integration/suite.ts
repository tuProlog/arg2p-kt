import * as assert from 'node:assert';
import * as path from 'node:path';
import * as vscode from 'vscode';

// The extension in VS Code: run by run.mjs, in a VS Code of its own (its user data and extensions apart)

// [check] holding within [timeout] ms, polled
async function eventually<T>(check: () => T | undefined, timeout = 90_000): Promise<T> {
    const start = Date.now();
    for (;;) {
        const value = check();
        if (value !== undefined) return value;
        if (Date.now() - start > timeout) throw new Error('timed out');
        await new Promise(resolve => setTimeout(resolve, 250));
    }
}

/** Called by VS Code (extensionTestsPath): fails by throwing */
export async function run(): Promise<void> {
    const fixture = vscode.Uri.file(path.join(__dirname, '..', 'test', 'integration', 'fixture', 'broken.arg2p'));
    const document = await vscode.workspace.openTextDocument(fixture);
    await vscode.window.showTextDocument(document);
    assert.strictEqual(document.languageId, 'arg2p');

    // the language server checks the theory: where it does not parse is a problem of the document
    const problems = await eventually(() => {
        const found = vscode.languages.getDiagnostics(fixture);
        return found.length > 0 ? found : undefined;
    });
    assert.strictEqual(problems.length, 1);
    assert.strictEqual(problems[0].range.start.line, 1);
    assert.strictEqual(problems[0].source, 'arg2p');

    // fixed, the problem goes
    const editor = vscode.window.activeTextEditor!;
    await editor.edit(edit => edit.replace(new vscode.Range(1, 0, 1, 200), 'r1 : bird(X) => flies(X).'));
    await eventually(() => (vscode.languages.getDiagnostics(fixture).length === 0 ? true : undefined));

    // the solver opens beside the theory, in a group of its own, and the theory keeps the focus
    await vscode.commands.executeCommand('arg2p.solve');
    const solver = await eventually(() => vscode.window.tabGroups.all.find(group => group.tabs.some(tab => tab.label === 'Solve broken.arg2p')));
    const theory = vscode.window.tabGroups.all.find(group => group.tabs.some(tab => tab.label === 'broken.arg2p'));
    assert.notStrictEqual(solver, theory);
    assert.strictEqual(vscode.window.activeTextEditor?.document, document);

    // the federation shows in its view, enabled by default
    assert.strictEqual(vscode.workspace.getConfiguration('arg2p.federation').get('enabled'), true);
    await vscode.commands.executeCommand('arg2p.federation');
    console.log('[arg2p] integration: diagnostics, edits, the solver panel and the federation view work');
}
