import { Bridge } from '../../src/bridge.js';

// The extension, as the page of the solver sees it, played in a browser: the bridge of the extension, asking the
// language server through server.mjs (POST /rpc). Loaded before the page (see server.mjs).

interface Preview {
    theory: string;
    flags: string;
}

const preview = (window as unknown as { ARG2P_PREVIEW: Preview }).ARG2P_PREVIEW;

const toPage = (message: unknown) => window.dispatchEvent(new MessageEvent('message', { data: message }));

async function rpc(method: string, params: unknown, notify = false): Promise<never> {
    const response = await fetch('/rpc', { method: 'POST', body: JSON.stringify({ method, params, notify }) });
    if (!response.ok) throw new Error(await response.text());
    return (await response.json()) as never;
}

const bridge = new Bridge(
    {
        request: (method, params) => rpc(method, params),
        notify: (method, params) => void rpc(method, params, true),
    },
    answer => toPage({ type: 'engine', answer }),
);

// what the server prints, polled
setInterval(async () => {
    const printed = (await fetch('/output').then(it => it.json())) as { id: number; text: string }[];
    printed.forEach(it => bridge.output(it));
}, 500);

let theory = preview.theory;
(window as unknown as { acquireVsCodeApi: () => unknown }).acquireVsCodeApi = () => ({
    postMessage(message: { type: string; request?: never; theory?: string }) {
        if (message.type === 'ready') toPage({ type: 'document', name: 'preview.arg2p', theory, query: 'buildLabelSets(In, Out, Und)', flags: preview.flags });
        else if (message.type === 'engine') void bridge.handle(message.request!);
        else if (message.type === 'edit') {
            theory = message.theory!;
            toPage({ type: 'theory', theory });
        }
    },
});
