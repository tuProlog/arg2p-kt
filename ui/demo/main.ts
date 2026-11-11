import '../src/index.js';
import { DEFAULT_FLAGS } from '../src/flags/schema.js';
import { JsEngine } from '../src/js/index.js';
import type { SyntaxProblem } from '../src/engine.js';

// A page to try the components on while they are written (npm run dev): not published

const EXAMPLES: Record<string, string> = {
    tweety: '% Tweety, the bird that does not fly\nf1 :=> bird(tweety).\nr1 : bird(X) => flies(X).\nf2 :=> -flies(tweety).\n',
    aba: 'graphBuildMode(aba).\n\nassumption(a).\nassumption(b).\ncontrary(a, x).\n\nr1 : b -> x.\nr2 : a -> y.\n',
    abstract: 'graphBuildMode(abstract).\n\na.\nb.\nc.\na -> b.\nb <-> c.\n\nX -> Y :- beats(X, Y).\nbeats(d, a).\n',
};

const $ = <K extends keyof HTMLElementTagNameMap>(selector: string) => document.querySelector(selector) as HTMLElementTagNameMap[K];
const theory = $<'arg2p-editor'>('#theory');
const example = $<'select'>('#example');
const tabbed = $<'input'>('#tabbed');
const solver = $<'arg2p-solver'>('arg2p-solver');

// from the sources: the worker of the package is found next to the engine once built
const engine = new JsEngine(() => new Worker(new URL('../src/js/worker.ts', import.meta.url), { type: 'module' }) as never);
engine.start();
theory.engine = engine;
solver.engine = engine;
solver.flags = DEFAULT_FLAGS;
solver.query = 'arg2p::solve(In, Out, Und)';

const setTheory = (text: string) => {
    theory.value = text;
    solver.theory = text;
    solver.clear();
};
Object.keys(EXAMPLES).forEach(name => example.add(new Option(name, name)));
example.addEventListener('change', () => setTheory(EXAMPLES[example.value]));
setTheory(EXAMPLES.tweety);

// the theory edited beside the solver, or in its tab
tabbed.addEventListener('change', () => {
    solver.theoryTab = tabbed.checked;
    document.body.classList.toggle('tabbed', tabbed.checked);
});
theory.addEventListener('arg2p-change', event => (solver.theory = (event as CustomEvent<{ value: string }>).detail.value));
solver.addEventListener('arg2p-theory-change', event => (theory.value = (event as CustomEvent<{ theory: string }>).detail.theory));
solver.addEventListener('arg2p-theory-error', event => theory.report((event as CustomEvent<{ problem: SyntaxProblem }>).detail.problem));
solver.addEventListener('arg2p-run', event => console.log('run', (event as CustomEvent).detail));
$<'button'>('#dark').addEventListener('click', () => {
    document.documentElement.classList.toggle('dark');
    solver.restyle();
});
