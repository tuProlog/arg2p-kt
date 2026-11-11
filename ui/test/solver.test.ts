// @vitest-environment happy-dom
import { afterEach, assert, describe, it } from 'vitest';
import '../src/index.js';
import type { Arg2pEditor } from '../src/components/arg2p-editor.js';
import type { Arg2pFramework } from '../src/components/arg2p-framework.js';
import type { Arg2pGraph } from '../src/components/arg2p-graph.js';
import type { Arg2pSolver } from '../src/components/arg2p-solver.js';
import type { Engine, EngineError, GraphData, Run, SolutionData, SolveRequest, SyntaxProblem } from '../src/engine.js';
import { drawingOf } from '../src/graph/canvas.js';

const { deepEqual, equal } = assert;
const ok: (value: unknown, message?: string) => void = assert.ok;

// The simulated DOM looks for the element focused in another shadow tree past the document, and fails: it is none of
// this shadow tree
const activeElement = Object.getOwnPropertyDescriptor(ShadowRoot.prototype, 'activeElement')!;
Object.defineProperty(ShadowRoot.prototype, 'activeElement', {
    get() {
        try {
            return activeElement.get!.call(this);
        } catch {
            return null;
        }
    },
});

const GRAPH: GraphData = {
    profile: 'aspicPlus',
    arguments: [{ id: 'A0', descriptor: 'A0 : f1 : a', conclusion: 'a', topRule: 'none', label: 'in', supports: [] }],
    attacks: [],
};

const ABSTRACT: GraphData = {
    profile: 'abstract',
    arguments: ['a', 'b'].map((name, i) => ({ id: `A${i}`, descriptor: '', conclusion: name, topRule: name, label: i === 0 ? 'in' : 'out', supports: [] })),
    attacks: [{ from: 'A0', to: 'A1', type: 'abstract', on: 'A1' }],
};

const STOPPED: EngineError = { name: 'Stopped', message: 'The query was stopped.', syntax: null, stopped: true };

function yes(graph = GRAPH): SolutionData {
    return { result: 'yes', query: 'q', bindings: [{ name: 'X', value: 'a' }], error: null, graph, branch: 0 };
}

/**
 * An engine whose runs give [solutions], or fail with [failure]; held, what it answers waits to be let go, and
 * stopping fails what waits
 */
class Fake implements Engine {
    readonly requests: SolveRequest[] = [];
    stops = 0;
    private readonly waiting: { go(): void; fail(error: EngineError): void }[] = [];

    constructor(
        private readonly solutions: SolutionData[],
        private readonly failure: EngineError | null = null,
        public hold = false,
    ) {}

    solve(request: SolveRequest, output: (text: string) => void = () => {}): Promise<Run> {
        this.requests.push(request);
        output('printed');
        if (this.failure !== null) return Promise.reject(this.failure);
        let next = 0;
        const run: Run = {
            query: request.query,
            hasNext: () => next < this.solutions.length,
            next: () => this.answer(this.solutions[next++]),
            graphAt: () => Promise.resolve(GRAPH),
        };
        return this.answer(run);
    }

    check(): Promise<SyntaxProblem | null> {
        return Promise.resolve(null);
    }

    settings(): Promise<Record<string, string[]>> {
        return Promise.resolve({});
    }

    stop(): void {
        this.stops++;
        this.waiting.splice(0).forEach(it => it.fail(STOPPED));
    }

    /** Lets go what waits */
    go(): void {
        this.waiting.splice(0).forEach(it => it.go());
    }

    private answer<T>(value: T): Promise<T> {
        if (!this.hold) return Promise.resolve(value);
        return new Promise((resolve, reject) => this.waiting.push({ go: () => resolve(value), fail: reject }));
    }
}

async function solver(engine: Engine, setup: (it: Arg2pSolver) => void = () => {}) {
    const element = document.createElement('arg2p-solver') as Arg2pSolver;
    element.engine = engine;
    element.theory = 'f1 :=> a.';
    element.query = 'solve(In, Out, Und)';
    element.flags = 'queryMode.';
    setup(element);
    document.body.appendChild(element);
    await element.updateComplete;
    const events: { type: string; detail: any }[] = [];
    for (const type of ['arg2p-run', 'arg2p-busy', 'arg2p-error', 'arg2p-theory-error', 'arg2p-query-change', 'arg2p-flags-change', 'arg2p-theory-change', 'arg2p-change', 'arg2p-tab-change']) {
        element.addEventListener(type, event => events.push({ type, detail: (event as CustomEvent).detail }));
    }
    const root = element.shadowRoot!;
    return { element, events, root, find: <T extends Element>(selector: string) => root.querySelector<T>(selector)! };
}

const settled = (ms = 0) => new Promise(resolve => setTimeout(resolve, ms));

afterEach(() => {
    document.body.replaceChildren();
});

describe('<arg2p-solver>', function () {
    it('should solve the query, showing its solutions, the graph of the one shown, and what it printed', async function () {
        const engine = new Fake([yes()]);
        const { element, events, find } = await solver(engine);
        await element.run();
        await element.updateComplete;
        deepEqual(engine.requests, [{ theory: 'f1 :=> a.', query: 'solve(In, Out, Und)', flags: 'queryMode.' }]);
        deepEqual(events.find(it => it.type === 'arg2p-run')?.detail, engine.requests[0]);
        ok(find('arg2p-solutions').shadowRoot!.textContent!.includes('Solution 1'));
        equal(find<Arg2pGraph>('arg2p-graph').solution?.solution.result, 'yes');
        deepEqual(drawingOf(find('arg2p-graph'))!.nodes().map(it => it.id()), ['A0']);
        ok(find('.tabs').textContent!.includes('Console •'));
        equal(find('.console pre').textContent, 'printed');
        deepEqual(
            events.filter(it => it.type === 'arg2p-busy').map(it => it.detail.busy),
            [true, false],
        );
        equal(element.busy, false);
    });

    it('should be busy until the first solution, and stop when asked', async function () {
        const engine = new Fake([yes()], null, true);
        const { element, events } = await solver(engine);
        const running = element.run();
        await settled();
        await element.updateComplete;
        ok(element.busy);
        ok(element.hasAttribute('busy'));
        element.shadowRoot!.querySelector<HTMLButtonElement>('.stop')!.click();
        equal(engine.stops, 1);
        await running;
        await element.updateComplete;
        equal(element.busy, false);
        equal(events.filter(it => it.type === 'arg2p-error').length, 1);
        // a run stopped as it started shows why
        ok(element.shadowRoot!.querySelector('arg2p-solutions')!.shadowRoot!.textContent!.includes('Stopped'));
    });

    it('should tell a solution stopped, and be busy no more', async function () {
        const engine = new Fake([yes(), yes()], null, false);
        const { element, events, find } = await solver(engine);
        await element.run();
        engine.hold = true;
        find<HTMLElement>('arg2p-solutions').shadowRoot!.querySelector<HTMLButtonElement>('.next')!.click();
        await element.updateComplete;
        ok(element.busy);
        element.stop();
        await settled();
        await element.updateComplete;
        equal(element.busy, false);
        ok(find('.notice').textContent!.includes('Stopped before the next solution'));
        deepEqual(events.filter(it => it.type === 'arg2p-error').map(it => it.detail.error), [STOPPED]);
    });

    it('should let go a run cleared while it runs: busy no more, and stopped', async function () {
        const engine = new Fake([yes()], null, true);
        const { element, find } = await solver(engine);
        const running = element.run();
        await settled();
        ok(element.busy);
        element.clear();
        equal(element.busy, false);
        equal(engine.stops, 1);
        await running;
        await element.updateComplete;
        equal(element.busy, false);
        equal(find<HTMLElement>('arg2p-solutions').shadowRoot!.querySelector('.solution'), null);
        ok(find('.run'));
        // and one cleared while it computes a solution
        const next = new Fake([yes(), yes()]);
        element.engine = next;
        await element.run();
        next.hold = true;
        find<HTMLElement>('arg2p-solutions').shadowRoot!.querySelector<HTMLButtonElement>('.next')!.click();
        ok(element.busy);
        element.clear();
        equal(next.stops, 1);
        await settled();
        await element.updateComplete;
        equal(element.busy, false);
        equal(element.shadowRoot!.querySelector('.notice'), null);
    });

    it('should let go a run replaced while it starts: stopped once, what it answers late dropped', async function () {
        const engine = new Fake([yes()], null, true);
        // a stop that fails nothing: the run replaced answers late
        engine.stop = () => void engine.stops++;
        const { element, events, find } = await solver(engine);
        const first = element.run();
        await settled();
        const second = element.run();
        await settled();
        equal(engine.stops, 1);
        // the starts, then the first solution of the run that replaced the other
        engine.go();
        await settled();
        engine.go();
        await Promise.all([first, second]);
        await settled();
        await element.updateComplete;
        equal(engine.requests.length, 2);
        equal(events.filter(it => it.type === 'arg2p-run').length, 2);
        const cards = find<HTMLElement>('arg2p-solutions').shadowRoot!.querySelectorAll('.solution');
        equal(cards.length, 1);
        ok(cards[0].textContent!.includes('Solution 1'));
        equal(events.some(it => it.type === 'arg2p-error'), false);
        equal(element.busy, false);
        equal(engine.stops, 1);
    });

    it('should stop a run under way once taken out of the page', async function () {
        const engine = new Fake([yes()], null, true);
        const { element } = await solver(engine);
        const running = element.run();
        await settled();
        ok(element.busy);
        element.remove();
        equal(engine.stops, 1);
        equal(element.busy, false);
        await running;
        // one done is left as it is
        const done = new Fake([yes()]);
        const again = await solver(done);
        await again.element.run();
        again.element.remove();
        equal(done.stops, 0);
    });

    it('should leave the engine be when it is cleared with nothing running', async function () {
        const engine = new Fake([yes()]);
        const { element } = await solver(engine);
        await element.run();
        element.clear();
        equal(engine.stops, 0);
    });

    it('should reveal where the query does not parse', async function () {
        const syntax = { line: 1, column: 6, length: 1, message: "Unexpected token ')'", where: 'query' as const };
        const { element, events, find } = await solver(new Fake([], { name: 'Query error', message: 'm', syntax }));
        await element.run();
        await element.updateComplete;
        const query = find<Arg2pEditor>('#query');
        await query.updateComplete;
        ok(query.shadowRoot!.querySelector('.problem')!.textContent!.includes("Unexpected token ')'"));
        equal(events.find(it => it.type === 'arg2p-error')?.detail.error.syntax, syntax);
    });

    it('should reveal where the flags do not parse, in the flags as text', async function () {
        const syntax = { line: 1, column: 1, length: 1, message: 'Unexpected end of file', where: 'flags' as const };
        const { element, find } = await solver(new Fake([], { name: 'Flags error', message: 'm', syntax }));
        await element.run();
        await element.updateComplete;
        await element.updateComplete;
        equal(find<HTMLElement>('.panel.flags').hidden, false);
        ok(find<HTMLDetailsElement>('.flags-text').open);
        ok(find<Arg2pEditor>('#flags').shadowRoot!.querySelector('.problem')!.textContent!.includes('Unexpected end of file'));
    });

    it('should reveal where the theory does not parse in its tab, or else tell the page', async function () {
        const syntax = { line: 1, column: 8, length: 1, message: 'Unexpected end of file', where: 'theory' as const };
        const failure = { name: 'Theory error', message: 'm', syntax };
        const without = await solver(new Fake([], failure));
        await without.element.run();
        deepEqual(without.events.find(it => it.type === 'arg2p-theory-error')?.detail, { problem: syntax });
        equal(without.root.querySelector('#theory'), null);

        const tabbed = await solver(new Fake([], failure), it => (it.theoryTab = true));
        await tabbed.element.run();
        await tabbed.element.updateComplete;
        await tabbed.element.updateComplete;
        equal(tabbed.find<HTMLElement>('.panel.theory').hidden, false);
        ok(tabbed.find<Arg2pEditor>('#theory').shadowRoot!.querySelector('.problem')!.textContent!.includes('Unexpected end of file'));
        equal(tabbed.events.some(it => it.type === 'arg2p-theory-error'), false);
    });

    it('should draw an abstract framework, labelled by the solution, and tell the theory its drawing writes', async function () {
        const theory = 'graphBuildMode(abstract).\na.\nb.\na -> b.\n';
        const { element, events, find } = await solver(new Fake([yes(ABSTRACT)]), it => (it.theory = theory));
        ok(find<HTMLElement>('arg2p-graph').hidden);
        equal(find<HTMLElement>('arg2p-framework').hidden, false);
        ok(find('.profile').textContent!.includes('Abstract framework'));
        await element.run();
        await element.updateComplete;
        const drawing = find<Arg2pFramework>('arg2p-framework');
        await drawing.updateComplete;
        equal(drawingOf(drawing)!.getElementById('b').data('tone'), 'out');
        // drawn: an argument deleted
        drawingOf(drawing)!.getElementById('b').select();
        await drawing.updateComplete;
        [...drawing.shadowRoot!.querySelectorAll<HTMLButtonElement>('.bar button')].find(it => it.textContent === 'Delete')!.click();
        const told = events.filter(it => it.type === 'arg2p-theory-change');
        deepEqual(told.map(it => it.detail), [{ theory: 'graphBuildMode(abstract).\na.\n' }]);
        equal(element.theory, 'graphBuildMode(abstract).\na.\n');
    });

    it('should tell the query and the flags as they are edited, once', async function () {
        const { events, find } = await solver(new Fake([]));
        find<Arg2pEditor>('#query').dispatchEvent(new CustomEvent('arg2p-change', { detail: { value: 'q(X)' }, bubbles: true, composed: true }));
        find('arg2p-flags').dispatchEvent(new CustomEvent('arg2p-flags-change', { detail: { flags: 'metaRules.' }, bubbles: true, composed: true }));
        deepEqual(
            events.map(it => it.type),
            ['arg2p-query-change', 'arg2p-flags-change'],
        );
        deepEqual(events[0].detail, { query: 'q(X)' });
        deepEqual(events[1].detail, { flags: 'metaRules.' });
    });

    it('should show the tabs of the page after its own, the one picked alone', async function () {
        const { element, events, find } = await solver(new Fake([yes()]), it => {
            it.innerHTML = '<div slot="tab" id="mine" label="Mine">mine</div><div slot="tab" id="other" label="Other">other</div>';
            it.tab = 'mine';
        });
        const [mine, other] = [...element.children] as HTMLElement[];
        const labels = () => [...find('.tabs').querySelectorAll('[role=tab]')].map(it => it.textContent!.trim());
        deepEqual(labels(), ['Solutions', 'Flags', 'Console', 'Mine', 'Other']);
        equal(find('[aria-selected=true]').textContent!.trim(), 'Mine');
        deepEqual([mine.hidden, other.hidden], [false, true]);
        equal(mine.getAttribute('role'), 'tabpanel');
        ok(find<HTMLElement>('.panel.results').hidden);
        // picked: the page is told
        [...find('.tabs').querySelectorAll<HTMLButtonElement>('[role=tab]')].find(it => it.textContent!.trim() === 'Other')!.click();
        await element.updateComplete;
        deepEqual([mine.hidden, other.hidden], [true, false]);
        deepEqual(events.filter(it => it.type === 'arg2p-tab-change').map(it => it.detail), [{ tab: 'other' }]);
        // its label changed, and a tab gone: the solver follows
        other.setAttribute('label', 'Other (2)');
        mine.remove();
        await settled();
        await element.updateComplete;
        deepEqual(labels(), ['Solutions', 'Flags', 'Console', 'Other (2)']);
        // a run shows the solutions
        await element.run();
        await element.updateComplete;
        equal(element.tab, 'results');
        ok(other.hidden);
        equal(find<HTMLElement>('.panel.results').hidden, false);
        // a tab that is not there shows the solutions
        element.tab = 'none';
        await element.updateComplete;
        equal(find<HTMLElement>('.panel.results').hidden, false);
    });

    it('should be tabs as WAI-ARIA has them: each with its panel, picked with the arrows', async function () {
        const { element, events, find, root } = await solver(new Fake([]), it => {
            it.innerHTML = '<div slot="tab" id="mine" label="Mine">mine</div>';
        });
        const tabs = () => [...root.querySelectorAll<HTMLButtonElement>('[role=tab]')];
        deepEqual(
            tabs().map(it => it.getAttribute('aria-controls')),
            ['panel-results', 'panel-flags', 'panel-console', 'panel-hosted'],
        );
        equal(find('#panel-flags').getAttribute('aria-labelledby'), 'tab-flags');
        equal(element.querySelector('#mine')!.getAttribute('aria-label'), 'Mine');
        // the tab picked alone is reached with Tab
        deepEqual(tabs().map(it => it.tabIndex), [0, -1, -1, -1]);
        const key = (key: string) => root.activeElement!.dispatchEvent(new KeyboardEvent('keydown', { key, bubbles: true, composed: true }));
        tabs()[0].focus();
        key('ArrowRight');
        await element.updateComplete;
        equal(element.tab, 'flags');
        equal(root.activeElement, tabs()[1]);
        deepEqual(tabs().map(it => it.tabIndex), [-1, 0, -1, -1]);
        key('End');
        await element.updateComplete;
        equal(element.tab, 'mine');
        key('ArrowRight');
        await element.updateComplete;
        equal(element.tab, 'results');
        key('ArrowLeft');
        await element.updateComplete;
        equal(element.tab, 'mine');
        deepEqual(events.filter(it => it.type === 'arg2p-tab-change').map(it => it.detail.tab), ['flags', 'mine', 'results', 'mine']);
    });

    it('should share the places of the arguments between the graph and the drawing', async function () {
        const theory = 'graphBuildMode(abstract).\na.\nb.\na -> b.\n';
        const { element, find } = await solver(new Fake([yes(ABSTRACT)]), it => (it.theory = theory));
        const drawing = find<Arg2pFramework>('arg2p-framework');
        drawingOf(drawing)!.getElementById('a').position({ x: 7, y: 9 }).emit('dragfree');
        await element.updateComplete;
        deepEqual(find<Arg2pGraph>('arg2p-graph').positions!.a, { x: 7, y: 9 });
    });
});

// A fake whose runs print as they give each solution
class Printing extends Fake {
    override solve(request: SolveRequest, output: (text: string) => void = () => {}): Promise<Run> {
        return super.solve(request, output).then(run => ({
            query: run.query,
            hasNext: () => run.hasNext(),
            next: () => {
                output('+');
                return run.next();
            },
            graphAt: (branch: number) => run.graphAt(branch),
        }));
    }
}

describe('<arg2p-solver> saved and restored', function () {
    it('should show the results of another theory, and the first ones again as they were, going on from there', async function () {
        const first = new Printing([yes(), yes(), yes()]);
        const { element, find } = await solver(first);
        await element.run();
        await find<HTMLElement & { next(): Promise<void> }>('arg2p-solutions').next();
        await element.updateComplete;
        const saved = element.save();
        equal(saved.solutions.entries.length, 2);
        element.restore(null);
        await element.updateComplete;
        equal(find('arg2p-solutions').shadowRoot!.textContent!.includes('Solution 1'), false);
        equal(find<Arg2pGraph>('arg2p-graph').solution, null);
        element.restore(saved);
        await element.updateComplete;
        const text = () => find('arg2p-solutions').shadowRoot!.textContent!;
        ok(text().includes('Solution 1') && text().includes('Solution 2'));
        equal(find<Arg2pGraph>('arg2p-graph').solution?.solution.result, 'yes');
        equal(find('.console pre').textContent, 'printed++');
        // the run goes on: the third solution
        await find<HTMLElement & { next(): Promise<void> }>('arg2p-solutions').next();
        await element.updateComplete;
        ok(text().includes('Solution 3'));
        equal(first.requests.length, 1, 'not run again');
        equal(find('.console pre').textContent, 'printed+++', 'what the run prints goes on showing');
    });

    it('should stop a run under way when another one is shown', async function () {
        const engine = new Fake([yes(), yes()]);
        const { element } = await solver(engine);
        await element.run();
        engine.hold = true;
        void element.shadowRoot!.querySelector<HTMLElement & { next(): Promise<void> }>('arg2p-solutions')!.next();
        await element.updateComplete;
        ok(element.busy);
        element.restore(null);
        equal(engine.stops, 1);
        equal(element.busy, false);
    });
});
