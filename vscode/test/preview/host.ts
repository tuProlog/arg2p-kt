import { Bridge } from '../../src/bridge.js';
import { FLAGS, QUERY } from '../../src/defaults.js';
import type { FromPage, ToPage } from '../../src/protocol.js';

// The extension, as the page of the solver sees it, played in a browser: the bridge of the extension, asking the
// language server through server.mjs (POST /rpc). Loaded before the page (see server.mjs).

interface Preview {
    theory: string;
}

const preview = (window as unknown as { ARG2P_PREVIEW: Preview }).ARG2P_PREVIEW;

const toPage = (message: ToPage) => window.dispatchEvent(new MessageEvent('message', { data: message }));

async function rpc(method: string, params: unknown): Promise<never> {
    const response = await fetch('/rpc', { method: 'POST', body: JSON.stringify({ method, params }) });
    if (!response.ok) throw new Error(await response.text());
    return (await response.json()) as never;
}

const bridge = new Bridge({ request: rpc }, answer => toPage({ type: 'engine', answer }));

let theory = preview.theory;
(window as unknown as { acquireVsCodeApi: () => unknown }).acquireVsCodeApi = () => ({
    postMessage(message: FromPage) {
        if (message.type === 'ready') toPage({ type: 'document', name: 'preview.arg2p', theory, query: QUERY, flags: FLAGS, parallel: false });
        else if (message.type === 'engine') void bridge.handle(message.request);
        // the switch Parallel, as the panel takes it
        else if (message.type === 'state') bridge.parallel = message.parallel;
        else if (message.type === 'edit') {
            theory = message.theory;
            toPage({ type: 'theory', theory });
        }
        // the extension shows it in the document, which the preview has not
        else if (message.type === 'problem') console.warn(`The theory does not parse at line ${message.problem.line}: ${message.problem.message}`);
    },
});
