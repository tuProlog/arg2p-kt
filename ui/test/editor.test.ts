// @vitest-environment happy-dom
import { forceLinting } from '@codemirror/lint';
import { Text } from '@codemirror/state';
import { runScopeHandlers } from '@codemirror/view';
import { afterEach, assert, describe, it } from 'vitest';
import '../src/index.js';
import type { Arg2pEditor } from '../src/components/arg2p-editor.js';
import { rangeOf, viewOf } from '../src/editor/view.js';
import type { SyntaxProblem } from '../src/engine.js';

const { deepEqual, equal } = assert;
const ok: (value: unknown, message?: string) => void = assert.ok;

async function editor(kind: 'theory' | 'query', value: string, problem: SyntaxProblem | null = null, check = (_: string) => Promise.resolve(problem)) {
    const element = document.createElement('arg2p-editor') as Arg2pEditor;
    const checked: string[] = [];
    element.kind = kind;
    element.value = value;
    element.label = 'Theory';
    element.engine = {
        check: text => {
            checked.push(text);
            return check(text);
        },
    };
    document.body.appendChild(element);
    await element.updateComplete;
    const events: { type: string; value: string }[] = [];
    for (const type of ['arg2p-change', 'arg2p-submit']) {
        element.addEventListener(type, event => events.push({ type, value: (event as CustomEvent).detail.value }));
    }
    return { element, view: viewOf(element)!, checked, events };
}

const settled = (ms = 50) => new Promise(resolve => setTimeout(resolve, ms));

afterEach(() => {
    document.body.replaceChildren();
});

describe('<arg2p-editor>', function () {
    it('should edit its value, and tell the changes typed', async function () {
        const { element, view, events } = await editor('theory', 'a :=> b.');
        equal(view.state.doc.toString(), 'a :=> b.');
        view.dispatch({ changes: { from: view.state.doc.length, insert: '\nc :=> d.' } });
        deepEqual(events, [{ type: 'arg2p-change', value: 'a :=> b.\nc :=> d.' }]);
        equal(element.value, 'a :=> b.\nc :=> d.');
        // a value set from outside is shown, and not told back
        element.value = 'e :=> f.';
        await element.updateComplete;
        equal(view.state.doc.toString(), 'e :=> f.');
        equal(events.length, 1);
    });

    it('should keep a query on one line, and submit it with Enter', async function () {
        const { view, events } = await editor('query', 'flies(X)');
        view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: 'solve(\n  In, Out,\n  Und)' } });
        equal(view.state.doc.toString(), 'solve( In, Out, Und)');
        runScopeHandlers(view, new KeyboardEvent('keydown', { key: 'Enter' }), 'editor');
        deepEqual(events.at(-1), { type: 'arg2p-submit', value: 'solve( In, Out, Und)' });
    });

    it('should show where the text does not parse, as the engine tells', async function () {
        const problem = { line: 2, column: 7, length: 1, message: "Unexpected token '('" };
        const { element, view, checked } = await editor('theory', 'a :=> b.\nc :=> d(.', problem);
        forceLinting(view);
        await settled();
        await element.updateComplete;
        deepEqual(checked, ['a :=> b.\nc :=> d(.']);
        const strip = element.shadowRoot!.querySelector('.problem')!;
        ok(strip.textContent!.includes('Theory, line 2, col 7'));
        ok(strip.textContent!.includes("Unexpected token '('"));
        // tapped, it takes the editor there
        (strip as HTMLElement).click();
        equal(view.state.selection.main.head, 'a :=> b.\n'.length + 6);
    });

    it('should report a problem found by running at once', async function () {
        const { element, view } = await editor('theory', 'a :=> b');
        element.report({ line: 1, column: 8, length: 1, message: 'Unexpected end of file' });
        await element.updateComplete;
        ok(element.shadowRoot!.querySelector('.problem')!.textContent!.includes('Unexpected end of file'));
        equal(view.state.selection.main.head, 7);
    });

    it('should leave a problem as it is when the text changed while it was checked, or one was reported', async function () {
        const answers: ((problem: SyntaxProblem | null) => void)[] = [];
        const { element, view } = await editor('theory', 'a :=> b(.', null, () => new Promise(resolve => answers.push(resolve)));
        forceLinting(view);
        await settled();
        // a problem found by running, while the check is under way
        element.report({ line: 1, column: 9, length: 1, message: 'Unexpected end of file' });
        answers[0](null);
        await settled();
        await element.updateComplete;
        ok(element.shadowRoot!.querySelector('.problem')!.textContent!.includes('Unexpected end of file'));
        // the text changed, while another check is under way
        view.dispatch({ changes: { from: 0, insert: '% x\n' } });
        forceLinting(view);
        await settled();
        view.dispatch({ changes: { from: 0, insert: '% y\n' } });
        answers[1]({ line: 1, column: 2, length: 1, message: 'stale' });
        await settled();
        await element.updateComplete;
        equal(element.shadowRoot!.querySelector('.problem')!.textContent!.includes('stale'), false);
    });

    it('should follow its kind and its label once drawn', async function () {
        const { element, view, events } = await editor('theory', 'flies(X)');
        equal(view.contentDOM.getAttribute('aria-label'), 'Theory');
        element.kind = 'query';
        element.label = 'Query';
        await element.updateComplete;
        equal(view.contentDOM.getAttribute('aria-label'), 'Query');
        runScopeHandlers(view, new KeyboardEvent('keydown', { key: 'Enter' }), 'editor');
        deepEqual(events.at(-1), { type: 'arg2p-submit', value: 'flies(X)' });
    });

    it('should edit its value again once put back in a page', async function () {
        const { element } = await editor('theory', 'a :=> b.');
        element.remove();
        equal(viewOf(element), null);
        element.value = 'c :=> d.';
        document.body.appendChild(element);
        await element.updateComplete;
        equal(viewOf(element)!.state.doc.toString(), 'c :=> d.');
    });

    it('should place a problem within the text', function () {
        const doc = Text.of(['a :=> b.', 'cd']);
        deepEqual(rangeOf(doc, { line: 2, column: 2, length: 5 }), { from: 10, to: 11 });
        deepEqual(rangeOf(doc, { line: 9, column: 9, length: 1 }), { from: 11, to: 11 });
    });
});
