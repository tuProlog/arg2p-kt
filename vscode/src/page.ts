import { randomBytes } from 'node:crypto';
import * as vscode from 'vscode';

/** The page of a webview: [body], and the bundle [script] of [media] running it, under a policy letting nothing else run */
export function page(webview: vscode.Webview, media: vscode.Uri, script: string, body: string): string {
    const source = webview.asWebviewUri(vscode.Uri.joinPath(media, script));
    // a nonce no page can guess: random bytes of the system, not those of Math.random
    const nonce = randomBytes(16).toString('base64');
    const policy = [
        "default-src 'none'",
        `script-src 'nonce-${nonce}'`,
        `style-src ${webview.cspSource} 'unsafe-inline'`,
        `img-src ${webview.cspSource} data:`,
        `font-src ${webview.cspSource}`,
    ].join('; ');
    return `<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta http-equiv="Content-Security-Policy" content="${policy}">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Arg2P</title>
</head>
<body>
${body}
<script type="module" nonce="${nonce}" src="${source}"></script>
</body>
</html>`;
}
