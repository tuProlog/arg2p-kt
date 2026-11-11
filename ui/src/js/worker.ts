// The solver of the browser in a worker of its own (see JsEngine): the page stays responsive while it works, and an
// evaluation that runs too long is stopped by ending the worker.
import { createHost, type Request } from './host.js';

const scope = self as unknown as { postMessage(message: unknown): void; onmessage: ((event: MessageEvent) => void) | null };
const host = createHost(answer => scope.postMessage(answer));
scope.onmessage = event => host.handle(event.data as Request);
