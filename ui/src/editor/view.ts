import type { Text } from '@codemirror/state';
import type { EditorView } from '@codemirror/view';
import type { SyntaxProblem } from '../engine.js';

// The editors of the components (CodeMirror): where a problem is in a text, and the view of each editor.

/** Where [problem] is in [doc]: its token, held within the text */
export function rangeOf(doc: Text, problem: Pick<SyntaxProblem, 'line' | 'column' | 'length'>): { from: number; to: number } {
    const line = doc.line(Math.max(1, Math.min(problem.line, doc.lines)));
    const from = line.from + Math.max(0, Math.min(problem.column - 1, line.length));
    return { from, to: Math.max(from, Math.min(from + problem.length, line.to)) };
}

const views = new WeakMap<Element, EditorView>();

/** The view of [element], an editor, while it is in a page (null for none) */
export function keepView(element: Element, view: EditorView | null): void {
    if (view === null) views.delete(element);
    else views.set(element, view);
}

/** The view of [element], an editor: for tests, it is no part of the API */
export function viewOf(element: Element): EditorView | null {
    return views.get(element) ?? null;
}
