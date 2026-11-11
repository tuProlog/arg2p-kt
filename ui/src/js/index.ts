// The engine of the browser: the solver of @tuprolog/arg2p, in a worker of its own (import it apart from the
// components: the worker brings the solver with it, the page does not load it)
export { JsEngine, type Channel } from './engine.js';
export type { Request, Answer } from './host.js';
export { compactGraphData, graphData } from './graph.js';
