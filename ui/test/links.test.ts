// @vitest-environment happy-dom
import { afterEach, assert, describe, it } from 'vitest';
import '../src/index.js';
import type { Arg2pEditor, EditorMark } from '../src/components/arg2p-editor.js';
import type { Arg2pFramework } from '../src/components/arg2p-framework.js';
import type { Arg2pGraph } from '../src/components/arg2p-graph.js';
import { viewOf } from '../src/editor/view.js';
import type { GraphData, Solved } from '../src/engine.js';
import { drawingOf } from '../src/graph/canvas.js';
import { isLinked, passagesOf, sourceStatement, statementOf } from '../src/graph/drawing.js';

// What links the components to what a page picks: the statements arguments come from, the arguments outlined, the
// marks of an editor, and the events that tell what was picked

const { deepEqual, equal } = assert;
const ok: (value: unknown, message?: string) => void = assert.ok;

// Tweety, as arg2p 0.25 gives it: premises named in their descriptor, a strict rule transposed (r2_i)
const GRAPH: GraphData = {
    profile: 'aspic_plus',
    arguments: [
        { id: 'A0', descriptor: 'A0 : f1 : bird(tweety)', conclusion: 'bird(tweety)', topRule: 'none', label: 'in', supports: [] },
        { id: 'A1', descriptor: 'A1 : ax1 : penguin(tweety)', conclusion: 'penguin(tweety)', topRule: 'none', label: 'in', supports: [] },
        { id: 'A2', descriptor: 'A2 : A1,r2 : -flies(tweety)', conclusion: '-flies(tweety)', topRule: 'r2', label: 'in', supports: ['A1'] },
        { id: 'A3', descriptor: 'A3 : A0,r1 : flies(tweety)', conclusion: 'flies(tweety)', topRule: 'r1', label: 'out', supports: ['A0'] },
        { id: 'A4', descriptor: 'A4 : A3,r2_i : -penguin(tweety)', conclusion: '-penguin(tweety)', topRule: 'r2_i', label: 'out', supports: ['A3'] },
    ],
    attacks: [{ from: 'A2', to: 'A3', type: 'rebut', on: 'A3' }],
};

const shown = (graph: GraphData): Solved => ({ solution: { result: 'yes', query: 'q', bindings: [], error: null, graph, branch: 0 }, run: null });

async function element<T extends HTMLElement & { updateComplete: Promise<boolean> }>(tag: string, setup: (it: T) => void = () => {}): Promise<T> {
    const it = document.createElement(tag) as T;
    setup(it);
    document.body.appendChild(it);
    await it.updateComplete;
    return it;
}

// The details of the events [type] [target] tells
function told<T>(target: EventTarget, type: string): T[] {
    const details: T[] = [];
    target.addEventListener(type, event => details.push((event as CustomEvent<T>).detail));
    return details;
}

afterEach(() => {
    document.body.replaceChildren();
});

describe('Statements of arguments', function () {
    it('should take the top rule, the name of a premise, or an abstract argument itself', function () {
        deepEqual(
            GRAPH.arguments.map(it => statementOf(it, false)),
            ['f1', 'ax1', 'r2', 'r1', 'r2_i'],
        );
        equal(statementOf({ id: 'A0', descriptor: 'A0 : a : a', conclusion: 'a', topRule: 'a', label: 'in', supports: [] }, true), 'a');
    });

    it('should count a transposed rule as the rule it comes from', function () {
        equal(sourceStatement('r2_i'), 'r2');
        equal(sourceStatement('r2_i2'), 'r2');
        equal(sourceStatement('rule_inside'), 'rule_inside');
        ok(isLinked('r2_i', new Set(['r2'])));
        ok(!isLinked('r1', new Set(['r2'])));
    });
});

describe('<arg2p-graph> links', function () {
    it('should outline the arguments of the statements linked, transposed ones too', async function () {
        const graph = await element<Arg2pGraph>('arg2p-graph', it => (it.solution = shown(GRAPH)));
        const outlined = () => drawingOf(graph)!.nodes('.linked').map(it => it.id()).sort();
        deepEqual(outlined(), []);
        graph.linked = ['r2'];
        await graph.updateComplete;
        deepEqual(outlined(), ['A2', 'A4']);
        graph.linked = ['f1', 'ax1'];
        await graph.updateComplete;
        deepEqual(outlined(), ['A0', 'A1']);
    });

    it('should tell the argument tapped and its statement, and none off the arguments', async function () {
        const graph = await element<Arg2pGraph>('arg2p-graph', it => (it.solution = shown(GRAPH)));
        const picked = told<{ id: string | null; statement: string | null }>(graph, 'arg2p-argument-select');
        drawingOf(graph)!.getElementById('A0').emit('tap');
        drawingOf(graph)!.getElementById('A4').emit('tap');
        drawingOf(graph)!.emit('tap');
        deepEqual(picked, [
            { id: 'A0', statement: 'f1' },
            { id: 'A4', statement: 'r2_i' },
            { id: null, statement: null },
        ]);
    });
});

describe('<arg2p-graph> hovered, and the passages of its arguments', function () {
    it('should tell the argument hovered and its statement, and none once left', async function () {
        const graph = await element<Arg2pGraph>('arg2p-graph', it => (it.solution = shown(GRAPH)));
        const hovered = told<{ id: string | null; statement: string | null }>(graph, 'arg2p-argument-hover');
        drawingOf(graph)!.getElementById('A2').emit('mouseover');
        drawingOf(graph)!.getElementById('A2').emit('mouseout');
        deepEqual(hovered, [
            { id: 'A2', statement: 'r2' },
            { id: null, statement: null },
        ]);
    });

    it('should outline the arguments linked in place, without drawing them again', async function () {
        const graph = await element<Arg2pGraph>('arg2p-graph', it => (it.solution = shown(GRAPH)));
        const node = drawingOf(graph)!.getElementById('A2');
        node.position({ x: 999, y: 999 });
        graph.linked = ['r2'];
        await graph.updateComplete;
        ok(node.hasClass('linked'));
        deepEqual(node.position(), { x: 999, y: 999 });
    });

    it('should show in the popup of an argument the passages of its statement, or of the rule it is transposed from', async function () {
        deepEqual(passagesOf({ r2: ['Penguins are birds'] }, 'r2_i'), ['Penguins are birds']);
        deepEqual(passagesOf({}, 'r2'), []);
        const graph = await element<Arg2pGraph>('arg2p-graph', it => {
            it.solution = shown(GRAPH);
            it.passages = { r1: ['Birds normally fly'] };
        });
        drawingOf(graph)!.getElementById('A3').emit('tap');
        await graph.updateComplete;
        ok(graph.shadowRoot!.querySelector('.popup .passage')!.textContent!.includes('Birds normally fly'));
        drawingOf(graph)!.getElementById('A0').emit('tap');
        await graph.updateComplete;
        equal(graph.shadowRoot!.querySelector('.popup .passage'), null);
    });
});

describe('<arg2p-framework> links', function () {
    const THEORY = 'graphBuildMode(abstract).\na.\nb.\na -> b.\n';

    it('should outline the arguments linked, and tell the one tapped while selecting', async function () {
        const drawing = await element<Arg2pFramework>('arg2p-framework', it => (it.theory = THEORY));
        const picked = told<{ id: string | null; statement: string | null }>(drawing, 'arg2p-argument-select');
        drawing.linked = ['b'];
        await drawing.updateComplete;
        deepEqual(drawingOf(drawing)!.nodes('.linked').map(it => it.id()), ['b']);
        drawingOf(drawing)!.getElementById('a').emit('tap');
        deepEqual(picked, [{ id: 'a', statement: 'a' }]);
        drawing.passages = { a: ['A is said'] };
        await drawing.updateComplete;
        ok(drawing.shadowRoot!.querySelector('.popup .passage')!.textContent!.includes('A is said'));
    });
});

describe('<arg2p-editor> marks', function () {
    async function editor(kind: 'text' | 'theory', value: string, marks: EditorMark[] = []) {
        const it = await element<Arg2pEditor>('arg2p-editor', e => {
            e.kind = kind;
            e.value = value;
            e.marks = marks;
        });
        return { element: it, view: viewOf(it)! };
    }

    const TEXT = 'Birds fly. Penguins are birds that do not fly. Tweety is a penguin.';
    const MARKS: EditorMark[] = [
        { key: 'r1', from: 0, to: 10, tone: 0 },
        { key: 'r2', from: 11, to: 46, tone: 1, title: 'r2' },
        { key: 'f1', from: 47, to: 67, tone: 2, active: true },
    ];

    it('should edit a plain text, with no language, no line numbers and no check', async function () {
        let checked = 0;
        const { element: text, view } = await editor('text', TEXT);
        text.engine = { check: () => (checked++, Promise.resolve(null)) };
        await text.updateComplete;
        equal(view.state.doc.toString(), TEXT);
        equal(text.shadowRoot!.querySelector('.cm-lineNumbers'), null);
        await new Promise(resolve => setTimeout(resolve, 700));
        equal(checked, 0);
    });

    it('should mark the stretches given, the active ones standing out, and follow new ones', async function () {
        const { element: text } = await editor('text', TEXT, MARKS);
        const marked = () => [...text.shadowRoot!.querySelectorAll('.cm-arg2p-mark')].map(it => it.textContent);
        deepEqual(marked(), ['Birds fly.', 'Penguins are birds that do not fly.', 'Tweety is a penguin.']);
        deepEqual([...text.shadowRoot!.querySelectorAll('.cm-arg2p-mark-active')].map(it => it.textContent), ['Tweety is a penguin.']);
        text.marks = [{ key: 'r1', from: 0, to: 5, tone: 3 }];
        await text.updateComplete;
        deepEqual(marked(), ['Birds']);
    });

    it('should hold marks within the text, and ignore empty ones', async function () {
        const { element: text } = await editor('text', 'short', [
            { key: 'a', from: 2, to: 99, tone: 0 },
            { key: 'b', from: 3, to: 3, tone: 1 },
        ]);
        deepEqual([...text.shadowRoot!.querySelectorAll('.cm-arg2p-mark')].map(it => it.textContent), ['ort']);
    });

    it('should tell the mark the cursor goes into, the innermost one, and none once it leaves them', async function () {
        const { element: text, view } = await editor('theory', 'r1 : bird(X) => flies(X).\nf1 :=> bird(tweety).', [
            { key: 'r1', from: 0, to: 25, tone: 0 },
            { key: 'bird', from: 5, to: 12, tone: 1 },
            { key: 'f1', from: 26, to: 46, tone: 2 },
        ]);
        const picked = told<{ key: string | null }>(text, 'arg2p-mark-select');
        view.dispatch({ selection: { anchor: 2 } });
        view.dispatch({ selection: { anchor: 3 } });
        view.dispatch({ selection: { anchor: 7 } });
        view.dispatch({ selection: { anchor: 30 } });
        view.dispatch({ selection: { anchor: 25 } });
        deepEqual(picked.map(it => it.key), ['r1', 'bird', 'f1', 'r1']);
        text.showMark('f1');
        text.showMark('none');
    });

    it('should tell the mark the pointer goes over, and none once it leaves the text', async function () {
        const { element: text, view } = await editor('text', TEXT, MARKS);
        const hovered = told<{ key: string | null }>(text, 'arg2p-mark-hover');
        // happy-dom lays nothing out: where the pointer is, is where CodeMirror says
        const at = (pos: number | null) => (view.posAtCoords = (() => pos) as typeof view.posAtCoords);
        const move = () => view.contentDOM.dispatchEvent(new MouseEvent('mousemove', { bubbles: true, clientX: 1, clientY: 1 }));
        at(3);
        move();
        move();
        at(20);
        move();
        at(null);
        move();
        at(50);
        move();
        view.contentDOM.dispatchEvent(new MouseEvent('mouseleave'));
        deepEqual(hovered.map(it => it.key), ['r1', 'r2', null, 'f1', null]);
    });
});

describe('<arg2p-solver> solutions', function () {
    it('should fold away and back, telling the page', async function () {
        const solver = await element<HTMLElement & { updateComplete: Promise<boolean>; solutionsCollapsed: boolean }>('arg2p-solver');
        const toggled = told<{ collapsed: boolean }>(solver, 'arg2p-solutions-toggle');
        const handle = () => solver.shadowRoot!.querySelector<HTMLButtonElement>('.collapse')!;
        equal(handle().getAttribute('aria-expanded'), 'true');
        handle().click();
        await solver.updateComplete;
        ok(solver.hasAttribute('solutions-collapsed'));
        ok(solver.shadowRoot!.querySelector('.results')!.classList.contains('collapsed'));
        equal(handle().getAttribute('aria-expanded'), 'false');
        handle().click();
        await solver.updateComplete;
        ok(!solver.solutionsCollapsed);
        deepEqual(toggled, [{ collapsed: true }, { collapsed: false }]);
    });
});
