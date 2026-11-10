// @vitest-environment happy-dom
import { afterEach, assert, describe, it } from 'vitest';
import type { GraphData, Run, SolutionData, Solved } from '../src/engine.js';
import { drawingOf } from '../src/graph/canvas.js';
import '../src/index.js';
import type { Arg2pFramework } from '../src/components/arg2p-framework.js';
import type { Arg2pGraph } from '../src/components/arg2p-graph.js';
import type { Arg2pSolutions } from '../src/components/arg2p-solutions.js';
import { listItems } from '../src/formats/terms.js';

const { deepEqual, equal } = assert;
const ok: (value: unknown, message?: string) => void = assert.ok;

// a0 built on nothing, a1 on a0, a2 attacking a1
const GRAPH: GraphData = {
    profile: 'aspic_plus',
    arguments: [
        { id: 'A0', descriptor: 'A0 : f1 : bird(tweety)', conclusion: 'bird(tweety)', topRule: 'none', label: 'in', supports: [] },
        { id: 'A1', descriptor: 'A1 : A0,r1 : flies(tweety)', conclusion: 'flies(tweety)', topRule: 'r1', label: 'out', supports: ['A0'] },
        { id: 'A2', descriptor: 'A2 : f2 : -flies(tweety)', conclusion: '-flies(tweety)', topRule: 'none', label: 'in', supports: [] },
    ],
    attacks: [{ from: 'A2', to: 'A1', type: 'rebut', on: 'A1' }],
};

const ABSTRACT: GraphData = {
    profile: 'abstract',
    arguments: ['a', 'b', 'x'].map((name, i) => ({ id: `A${i}`, descriptor: `A${i} : ${name} : ${name}`, conclusion: name, topRule: name, label: i === 1 ? 'out' : 'in', supports: [] })),
    attacks: [
        { from: 'A0', to: 'A1', type: 'abstract', on: 'A1' },
        { from: 'A2', to: 'A1', type: 'abstract', on: 'A1' },
    ],
};

function solution(result: SolutionData['result'], bindings: SolutionData['bindings'] = [], graph = GRAPH): SolutionData {
    return { result, query: 'q', bindings, error: result === 'halt' ? 'boom' : null, graph, branch: 3 };
}

// [graph] as the graph of a solution, with no stages to browse
function shown(graph: GraphData): Solved {
    return { solution: { ...solution('yes'), graph, branch: 0 }, run: null };
}

// A run of [solutions], each given when asked for
function run(solutions: SolutionData[], stages: Map<number, GraphData> = new Map()): Run & { asked: number } {
    let next = 0;
    return {
        query: 'q',
        asked: 0,
        hasNext: () => next < solutions.length,
        next() {
            this.asked++;
            return Promise.resolve(solutions[next++]);
        },
        graphAt: branch => Promise.resolve(stages.get(branch) ?? GRAPH),
    };
}

async function element<T extends HTMLElement & { updateComplete: Promise<boolean> }>(tag: string): Promise<T> {
    const it = document.createElement(tag) as T;
    document.body.appendChild(it);
    await it.updateComplete;
    return it;
}

const settled = () => new Promise(resolve => setTimeout(resolve, 0));

afterEach(() => {
    document.body.replaceChildren();
});

describe('<arg2p-graph>', function () {
    it('should draw every argument and attack, and the supports, each argument above what it is built on', async function () {
        const graph = await element<Arg2pGraph>('arg2p-graph');
        ok(graph.empty);
        graph.solution = shown(GRAPH);
        await graph.updateComplete;
        const cy = drawingOf(graph)!;
        deepEqual(cy.nodes().map(it => it.id()).sort(), ['A0', 'A1', 'A2']);
        ok(cy.getElementById('A2->A1').nonempty());
        ok(cy.getElementById('A0=>A1').hasClass('support'));
        ok(cy.getElementById('A1').position().y < cy.getElementById('A0').position().y);
        equal(graph.empty, false);
    });

    it('should hide the supports and show the conclusions when asked, and tell it', async function () {
        const graph = await element<Arg2pGraph>('arg2p-graph');
        graph.solution = shown(GRAPH);
        await graph.updateComplete;
        const toggles: unknown[] = [];
        graph.addEventListener('arg2p-toggle', event => toggles.push((event as CustomEvent).detail));
        const [supports, conclusions] = graph.shadowRoot!.querySelectorAll<HTMLInputElement>('input[type=checkbox]');
        supports.checked = false;
        supports.dispatchEvent(new Event('change'));
        conclusions.checked = true;
        conclusions.dispatchEvent(new Event('change'));
        await graph.updateComplete;
        ok(drawingOf(graph)!.getElementById('A0=>A1').hasClass('hidden'));
        equal(drawingOf(graph)!.getElementById('A1').data('text'), 'A1\nflies(tweety)');
        deepEqual(toggles.at(-1), { showSupports: false, showConclusions: true });
    });

    it('should tell about an argument tapped', async function () {
        const graph = await element<Arg2pGraph>('arg2p-graph');
        graph.solution = shown(GRAPH);
        await graph.updateComplete;
        drawingOf(graph)!.getElementById('A1').emit('tap');
        await graph.updateComplete;
        const popup = graph.shadowRoot!.querySelector('.popup')!;
        ok(popup.textContent!.includes('flies(tweety)'));
        ok(popup.textContent!.includes('r1'));
        ok(popup.textContent!.includes('rebut'));
        drawingOf(graph)!.emit('tap');
        await graph.updateComplete;
        equal(graph.shadowRoot!.querySelector('.popup'), null);
    });

    it('should browse the stages of a computation, keeping the arguments in place', async function () {
        const graph = await element<Arg2pGraph>('arg2p-graph');
        const stage: GraphData = { ...GRAPH, arguments: GRAPH.arguments.slice(0, 1), attacks: [] };
        graph.solution = { solution: solution('yes'), run: run([], new Map([[2, stage]])) };
        await graph.updateComplete;
        const where = drawingOf(graph)!.getElementById('A0').position().x;
        equal(graph.shadowRoot!.querySelector('.stage')!.textContent, 'Stage 3 / 3');
        graph.shadowRoot!.querySelector<HTMLButtonElement>('.previous')!.click();
        await settled();
        await graph.updateComplete;
        equal(graph.shadowRoot!.querySelector('.stage')!.textContent, 'Stage 2 / 3');
        deepEqual(drawingOf(graph)!.nodes().map(it => it.id()), ['A0']);
        graph.shadowRoot!.querySelector<HTMLButtonElement>('.next')!.click();
        await graph.updateComplete;
        equal(graph.shadowRoot!.querySelector('.stage')!.textContent, 'Stage 3 / 3');
        equal(drawingOf(graph)!.getElementById('A0').position().x, where);
    });

    it('should place an abstract framework as its drawing does, and tell where its arguments are', async function () {
        const graph = await element<Arg2pGraph>('arg2p-graph');
        const told: Record<string, { x: number; y: number }>[] = [];
        graph.addEventListener('arg2p-positions', event => told.push((event as CustomEvent).detail.positions));
        graph.positions = { a: { x: 5, y: 6 }, b: { x: 100, y: 6 }, x: { x: 50, y: 90 } };
        graph.solution = shown(ABSTRACT);
        await graph.updateComplete;
        deepEqual(drawingOf(graph)!.getElementById('A1').position(), { x: 100, y: 6 });
        equal(told.length, 0);
        // moved by the drawing
        graph.positions = { a: { x: 5, y: 6 }, b: { x: 200, y: 6 }, x: { x: 50, y: 90 } };
        await graph.updateComplete;
        deepEqual(drawingOf(graph)!.getElementById('A1').position(), { x: 200, y: 6 });
        // a graph with no positions lays itself out, and tells
        const other = await element<Arg2pGraph>('arg2p-graph');
        other.addEventListener('arg2p-positions', event => told.push((event as CustomEvent).detail.positions));
        other.solution = shown(ABSTRACT);
        await other.updateComplete;
        deepEqual(Object.keys(told.at(-1)!).sort(), ['a', 'b', 'x']);
    });

    it('should keep the stage drawn when another cannot be read', async function () {
        const graph = await element<Arg2pGraph>('arg2p-graph');
        const stopped = { name: 'Stopped', message: 'stopped', syntax: null, stopped: true };
        graph.solution = { solution: solution('yes'), run: { graphAt: () => Promise.reject(stopped) } };
        await graph.updateComplete;
        graph.shadowRoot!.querySelector<HTMLButtonElement>('.previous')!.click();
        await settled();
        await graph.updateComplete;
        equal(graph.shadowRoot!.querySelector('.stage')!.textContent, 'Stage 3 / 3');
        deepEqual(drawingOf(graph)!.nodes().map(it => it.id()).sort(), ['A0', 'A1', 'A2']);
    });

    it('should leave the positions it is given as they are, and tell where the arguments are dragged', async function () {
        const graph = await element<Arg2pGraph>('arg2p-graph');
        const told: Record<string, { x: number; y: number }>[] = [];
        graph.addEventListener('arg2p-positions', event => told.push((event as CustomEvent).detail.positions));
        const given = { a: { x: 5, y: 6 }, b: { x: 100, y: 6 } };
        graph.positions = given;
        graph.solution = shown(ABSTRACT);
        await graph.updateComplete;
        // x was laid out, and told
        deepEqual(Object.keys(told.at(-1)!).sort(), ['a', 'b', 'x']);
        drawingOf(graph)!.getElementById('A0').position({ x: 40, y: 40 }).emit('dragfree');
        deepEqual(told.at(-1)!.a, { x: 40, y: 40 });
        equal(graph.positions, given);
    });

    it('should draw again once put back in a page', async function () {
        const graph = await element<Arg2pGraph>('arg2p-graph');
        graph.solution = shown(GRAPH);
        await graph.updateComplete;
        graph.remove();
        equal(drawingOf(graph), null);
        document.body.appendChild(graph);
        await graph.updateComplete;
        deepEqual(drawingOf(graph)!.nodes().map(it => it.id()).sort(), ['A0', 'A1', 'A2']);
        drawingOf(graph)!.getElementById('A1').emit('tap');
        await graph.updateComplete;
        ok(graph.shadowRoot!.querySelector('.popup')!.textContent!.includes('flies(tweety)'));
    });

    it('should draw nothing while out of a page, and what it was given once put back', async function () {
        const graph = await element<Arg2pGraph>('arg2p-graph');
        graph.remove();
        graph.solution = shown(GRAPH);
        await graph.updateComplete;
        equal(drawingOf(graph), null);
        document.body.appendChild(graph);
        await graph.updateComplete;
        deepEqual(drawingOf(graph)!.nodes().map(it => it.id()).sort(), ['A0', 'A1', 'A2']);
    });

    it('should colour the arguments by their labels, in the colours the page sets once restyled', async function () {
        const graph = await element<Arg2pGraph>('arg2p-graph');
        graph.solution = shown(GRAPH);
        await graph.updateComplete;
        equal(drawingOf(graph)!.getElementById('A1').data('color'), '#800000');
        graph.style.setProperty('--_out', '#ff0000');
        graph.restyle();
        equal(drawingOf(graph)!.getElementById('A1').data('color'), '#ff0000');
    });

    it('should name the arguments of an abstract framework as the theory does', async function () {
        const graph = await element<Arg2pGraph>('arg2p-graph');
        graph.solution = shown(ABSTRACT);
        await graph.updateComplete;
        deepEqual(drawingOf(graph)!.nodes().map(it => it.data('text')), ['a', 'b', 'x']);
        equal(drawingOf(graph)!.edges('.support').length, 0);
        // no supports nor conclusions to show
        equal(graph.shadowRoot!.querySelectorAll('input[type=checkbox]').length, 0);
    });
});

describe('<arg2p-solutions>', function () {
    it('should show the solutions one at a time, the newest shown', async function () {
        const solutions = await element<Arg2pSolutions>('arg2p-solutions');
        const shown: SolutionData[] = [];
        solutions.addEventListener('arg2p-solution', event => shown.push((event as CustomEvent).detail.solution));
        const yes = solution('yes', [{ name: 'In', value: '[[a], [b, c]]' }]);
        const queries = run([yes, solution('yes'), solution('no')]);
        await solutions.start(queries);
        await solutions.updateComplete;
        equal(queries.asked, 1);
        equal(shown.length, 1);
        equal(solutions.trivial, false);
        const chips = [...solutions.shadowRoot!.querySelectorAll('.chip.in')].map(it => it.textContent);
        deepEqual(chips, ['a', '[b, c]']);

        await solutions.next();
        await solutions.next();
        await solutions.updateComplete;
        const cards = solutions.shadowRoot!.querySelectorAll('.solution');
        equal(cards.length, 3);
        ok(cards[2].textContent!.includes('No more solutions'));
        ok(solutions.shadowRoot!.querySelector<HTMLButtonElement>('.next')!.disabled);

        // an earlier solution picked is shown again
        (cards[0] as HTMLElement).click();
        equal(shown.at(-1), yes);
    });

    it('should hide the no that only ends the solutions, when asked', async function () {
        const solutions = await element<Arg2pSolutions>('arg2p-solutions');
        solutions.hideExhaustedNo = true;
        await solutions.start(run([solution('yes'), solution('no')]));
        await solutions.next();
        await solutions.updateComplete;
        equal(solutions.shadowRoot!.querySelectorAll('.solution').length, 1);
        // a single yes with nothing bound is not worth showing
        ok(solutions.trivial);
        await solutions.start(run([solution('no')]));
        await solutions.updateComplete;
        ok(solutions.shadowRoot!.textContent!.includes('No solutions'));
    });

    it('should show an error, and tell when it is picked', async function () {
        const solutions = await element<Arg2pSolutions>('arg2p-solutions');
        const picked: unknown[] = [];
        solutions.addEventListener('arg2p-error-select', event => picked.push((event as CustomEvent).detail.error));
        const error = { name: 'Theory error', message: 'Unexpected token', syntax: { line: 1, column: 2, length: 1, message: 'm', where: 'theory' as const } };
        solutions.fail(error);
        await solutions.updateComplete;
        solutions.shadowRoot!.querySelector<HTMLElement>('.halt')!.click();
        deepEqual(picked, [error]);
    });

    it('should be picked with buttons, and tell readers of the screen what it is doing in a region there all along', async function () {
        const solutions = await element<Arg2pSolutions>('arg2p-solutions');
        const live = () => solutions.shadowRoot!.querySelector('[role=status]');
        ok(live());
        let release = () => {};
        const held: Run = { ...run([solution('yes'), solution('yes')]), next: () => new Promise(resolve => (release = () => resolve(solution('yes')))) };
        const first = solutions.start(held);
        await solutions.updateComplete;
        equal(live()!.textContent, 'Solving…');
        release();
        await first;
        await solutions.updateComplete;
        equal(live()!.textContent, '');
        const pick = solutions.shadowRoot!.querySelector<HTMLButtonElement>('.solution .pick')!;
        equal(pick.getAttribute('aria-pressed'), 'true');
        equal(solutions.shadowRoot!.querySelector('[role=button]'), null);
        // an error that tells where it is
        solutions.fail({ name: 'Theory error', message: 'm', syntax: { line: 1, column: 1, length: 1, message: 'm', where: 'theory' } });
        await solutions.updateComplete;
        ok(solutions.shadowRoot!.querySelector('.halt .pick'));
        ok(live());
    });

    it('should tell when a solution could not be computed', async function () {
        const solutions = await element<Arg2pSolutions>('arg2p-solutions');
        const failures: unknown[] = [];
        solutions.addEventListener('arg2p-failure', event => failures.push((event as CustomEvent).detail.error));
        const stopped = { name: 'Stopped', message: 'stopped', syntax: null, stopped: true };
        await solutions.start({ query: 'q', hasNext: () => true, next: () => Promise.reject(stopped), graphAt: () => Promise.reject(stopped) });
        deepEqual(failures, [stopped]);
        equal(solutions.busy, false);
    });

    it('should read lists of labelled things', function () {
        deepEqual(listItems('[a, f(b, c), [d]]'), ['a', 'f(b, c)', '[d]']);
        deepEqual(listItems('[]'), []);
        equal(listItems('[a | T]'), null);
        equal(listItems('a'), null);
    });
});

describe('<arg2p-framework>', function () {
    const THEORY = 'graphBuildMode(abstract).\na.\nb.\na -> b.\nX -> Y :- beats(X, Y).\nbeats(x, b).\n';

    async function framework(theory = THEORY): Promise<{ editor: Arg2pFramework; theories: string[] }> {
        const editor = await element<Arg2pFramework>('arg2p-framework');
        const theories: string[] = [];
        editor.addEventListener('arg2p-theory-change', event => theories.push((event as CustomEvent).detail.theory));
        editor.theory = theory;
        await editor.updateComplete;
        return { editor, theories };
    }

    it('should draw what the theory declares, and what its graph derives, dashed', async function () {
        const { editor } = await framework();
        const cy = drawingOf(editor)!;
        deepEqual(cy.nodes().map(it => it.id()), ['a', 'b']);
        deepEqual(cy.edges().map(it => it.id()), ['a->b']);
        editor.graph = ABSTRACT;
        await editor.updateComplete;
        ok(cy.getElementById('x').hasClass('derived'));
        ok(cy.getElementById('x->b').hasClass('derived'));
        equal(cy.getElementById('a').hasClass('derived'), false);
    });

    it('should save a picture once something is drawn, as the graph does', async function () {
        const { editor } = await framework('graphBuildMode(abstract).\n');
        const graph = await element<Arg2pGraph>('arg2p-graph');
        const png = (it: HTMLElement) => it.shadowRoot!.querySelector<HTMLButtonElement>('.png')!;
        ok(png(editor).disabled);
        ok(png(graph).disabled);
        editor.theory = THEORY;
        graph.solution = shown(GRAPH);
        await Promise.all([editor.updateComplete, graph.updateComplete]);
        equal(png(editor).disabled, false);
        equal(png(graph).disabled, false);
    });

    it('should write the arguments and attacks drawn into the theory', async function () {
        const { editor, theories } = await framework();
        const cy = drawingOf(editor)!;
        const [, argument, attack] = editor.shadowRoot!.querySelectorAll<HTMLButtonElement>('.bar button');
        argument.click();
        await editor.updateComplete;
        cy.emit({ type: 'tap', position: { x: 10, y: 10 } } as never);
        await editor.updateComplete;
        equal(theories.at(-1), THEORY.replace('a -> b.', 'a -> b.\na1.'));
        attack.click();
        await editor.updateComplete;
        cy.getElementById('a1').emit('tap');
        cy.getElementById('a').emit('tap');
        await editor.updateComplete;
        equal(theories.at(-1), THEORY.replace('a -> b.', 'a -> b.\na1.\na1 -> a.').replace('a1.\n', 'a1.\n'));
    });

    it('should delete what is selected, and only what is declared', async function () {
        const { editor, theories } = await framework();
        editor.graph = ABSTRACT;
        await editor.updateComplete;
        const cy = drawingOf(editor)!;
        cy.getElementById('b').select();
        cy.getElementById('x').select();
        await editor.updateComplete;
        const remove = [...editor.shadowRoot!.querySelectorAll<HTMLButtonElement>('.bar button')].find(it => it.textContent === 'Delete')!;
        remove.click();
        await editor.updateComplete;
        equal(theories.at(-1), 'graphBuildMode(abstract).\na.\nX -> Y :- beats(X, Y).\nbeats(x, b).\n');
    });

    it('should rename an argument, where the theory declares it', async function () {
        const { editor, theories } = await framework();
        drawingOf(editor)!.getElementById('b').emit('dbltap');
        await editor.updateComplete;
        const box = editor.shadowRoot!.querySelector<HTMLInputElement>('.rename')!;
        box.value = 'bee';
        box.dispatchEvent(new Event('input'));
        await editor.updateComplete;
        box.dispatchEvent(new KeyboardEvent('keydown', { key: 'Enter' }));
        await editor.updateComplete;
        equal(theories.at(-1), THEORY.replace('b.\na -> b.', 'bee.\na -> bee.'));
        ok(drawingOf(editor)!.getElementById('bee').nonempty());
    });

    it('should tell about an argument tapped: its label, what attacks it, what it attacks', async function () {
        const { editor } = await framework();
        editor.graph = ABSTRACT;
        await editor.updateComplete;
        drawingOf(editor)!.getElementById('b').emit('tap');
        await editor.updateComplete;
        const popup = editor.shadowRoot!.querySelector('.popup')!.textContent!.replace(/\s+/g, ' ');
        ok(popup.includes('out'), popup);
        ok(/Attacked by ?a ?x/.test(popup), popup);
        ok(/Attacks ?none/.test(popup), popup);
        drawingOf(editor)!.emit('tap');
        await editor.updateComplete;
        equal(editor.shadowRoot!.querySelector('.popup'), null);
    });

    it('should bend mutual attacks apart, so that both show', async function () {
        const { editor } = await framework('graphBuildMode(abstract).\na <-> b.\nb -> c.\n');
        const cy = drawingOf(editor)!;
        ok(cy.getElementById('a->b').hasClass('mutual'));
        ok(cy.getElementById('b->a').hasClass('mutual'));
        equal(cy.getElementById('b->c').hasClass('mutual'), false);
    });

    it('should place the arguments where the graph has them, and tell where they are', async function () {
        const { editor } = await framework();
        const told: Record<string, { x: number; y: number }>[] = [];
        editor.addEventListener('arg2p-positions', event => told.push((event as CustomEvent).detail.positions));
        editor.positions = { a: { x: 10, y: 20 }, b: { x: 300, y: 40 } };
        await editor.updateComplete;
        deepEqual(drawingOf(editor)!.getElementById('b').position(), { x: 300, y: 40 });
        // an argument added by text goes next to the others, and is told
        editor.theory = `${THEORY}c.\n`;
        await editor.updateComplete;
        deepEqual(Object.keys(told.at(-1)!).sort(), ['a', 'b', 'c']);
        deepEqual(told.at(-1)!.a, { x: 10, y: 20 });
    });

    it('should forget the labels of a graph once the theory changes', async function () {
        const { editor } = await framework();
        editor.graph = ABSTRACT;
        await editor.updateComplete;
        editor.theory = `${THEORY}c.\n`;
        await editor.updateComplete;
        equal(drawingOf(editor)!.getElementById('x').nonempty(), false);
    });

    it('should tell about an argument tapped when read only, and keep the arguments where they are', async function () {
        const { editor } = await framework();
        editor.readonly = true;
        editor.graph = ABSTRACT;
        await editor.updateComplete;
        drawingOf(editor)!.getElementById('b').emit('tap');
        await editor.updateComplete;
        ok(editor.shadowRoot!.querySelector('.popup')!.textContent!.includes('out'));
        equal(editor.shadowRoot!.querySelectorAll('.bar button').length, 1);
        ok(drawingOf(editor)!.autoungrabify());
        // still so once put back in a page
        editor.remove();
        document.body.appendChild(editor);
        await editor.updateComplete;
        ok(drawingOf(editor)!.autoungrabify());
        deepEqual(drawingOf(editor)!.nodes().map(it => it.id()), ['a', 'b', 'x']);
    });

    it('should colour the arguments again in the colours the page sets', async function () {
        const { editor } = await framework();
        editor.graph = ABSTRACT;
        await editor.updateComplete;
        equal(drawingOf(editor)!.getElementById('b').data('color'), '#800000');
        editor.style.setProperty('--_out', '#ff0000');
        editor.restyle();
        equal(drawingOf(editor)!.getElementById('b').data('color'), '#ff0000');
    });

    it('should draw a theory edited as text, the arguments kept where they were', async function () {
        const { editor } = await framework();
        const where = { ...drawingOf(editor)!.getElementById('a').position() };
        editor.theory = `${THEORY}c.\nc -> a.\n`;
        await editor.updateComplete;
        deepEqual(drawingOf(editor)!.getElementById('a').position(), where);
        ok(drawingOf(editor)!.getElementById('c->a').nonempty());
    });
});
