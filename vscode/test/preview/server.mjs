import * as esbuild from 'esbuild';
import { spawn } from 'node:child_process';
import { existsSync, readFileSync } from 'node:fs';
import { createServer } from 'node:http';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { createMessageConnection, StreamMessageReader, StreamMessageWriter } from 'vscode-jsonrpc/node';
import { argumentsOf, javaOf } from '../../src/java.ts';

// The page of the solver in a browser, on the language server (npm run preview): http://localhost:5400, the theory of
// the file given (Tweety otherwise). It needs the bundles and the server (npm run build, ./gradlew :vscode:vscodeServer).

const extension = join(dirname(fileURLToPath(import.meta.url)), '..', '..');
const theory = process.argv[2] ? readFileSync(process.argv[2], 'utf8') : 'f1 :=> bird(tweety).\nr1 : bird(X) => flies(X).\nf2 :=> -flies(tweety).\n';
const port = Number(process.env.PORT ?? 5400);

await esbuild.build({
    entryPoints: [join(extension, 'test', 'preview', 'host.ts')],
    outfile: join(extension, 'dist-test', 'preview.js'),
    bundle: true,
    platform: 'browser',
    format: 'esm',
    logLevel: 'warning',
});

const jar = join(extension, 'server', 'arg2p-lsp.jar');
if (!existsSync(jar)) throw new Error(`No server: build it with ./gradlew :vscode:vscodeServer (${jar})`);
// the java of the extension, the settings aside
const server = spawn(javaOf('', extension, process.env.JAVA_HOME), argumentsOf([], jar), { stdio: ['pipe', 'pipe', 'inherit'] });
const connection = createMessageConnection(new StreamMessageReader(server.stdout), new StreamMessageWriter(server.stdin));
connection.listen();
await connection.sendRequest('initialize', { processId: null, rootUri: null, capabilities: {} });

const page = `<!doctype html>
<html lang="en"><head><meta charset="utf-8"><title>Arg2P preview</title>
<style>
  /* the colours of the dark theme of VS Code, as a webview has them */
  :root { --vscode-editor-background: #1f1f1f; --vscode-foreground: #cccccc; --vscode-descriptionForeground: #9d9d9d;
    --vscode-panel-border: #2b2b2b; --vscode-textLink-foreground: #4daafc; --vscode-errorForeground: #f85149;
    --vscode-button-background: #0078d4; --vscode-button-foreground: #ffffff; --vscode-button-hoverBackground: #026ec1;
    --vscode-list-hoverBackground: #2a2d2e; --vscode-font-family: system-ui, sans-serif; --vscode-editor-font-family: Consolas, monospace;
    --vscode-font-size: 13px; }
</style>
<script>window.ARG2P_PREVIEW = ${JSON.stringify({ theory, flags: 'argumentLabellingMode(grounded).\nstatementLabellingMode(statement).\nqueryMode.\n' })};</script>
<script type="module" src="/preview.js"></script>
<script type="module" src="/webview.js"></script>
</head><body><arg2p-solver></arg2p-solver></body></html>`;

createServer(async (request, response) => {
    try {
        if (request.url === '/') return response.writeHead(200, { 'content-type': 'text/html' }).end(page);
        if (request.url === '/preview.js') return response.writeHead(200, { 'content-type': 'text/javascript' }).end(readFileSync(join(extension, 'dist-test', 'preview.js')));
        if (request.url === '/webview.js') return response.writeHead(200, { 'content-type': 'text/javascript' }).end(readFileSync(join(extension, 'dist', 'webview.js')));
        if (request.url === '/rpc') {
            let body = '';
            for await (const chunk of request) body += chunk;
            const { method, params } = JSON.parse(body);
            const result = await connection.sendRequest(method, params);
            return response.writeHead(200, { 'content-type': 'application/json' }).end(JSON.stringify(result ?? null));
        }
        response.writeHead(404).end();
    } catch (error) {
        response.writeHead(500).end(String(error));
    }
}).listen(port, () => console.log(`The solver page on the language server: http://localhost:${port}`));
