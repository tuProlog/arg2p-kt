// @vitest-environment happy-dom
/// <reference types="vite/client" />
import { parseSync } from 'vite';
import { assert, describe, it } from 'vitest';
// The solver in one file, as the build makes it: npm run build first (npm test at the root does)
import code from '../dist/bundle/solver.js?raw';

const { deepEqual } = assert;
const ok: (value: unknown, message?: string) => void = assert.ok;

describe('The solver in one file', function () {
    it('should import nothing: its dependencies are within', function () {
        const { module, errors } = parseSync('solver.js', code);
        deepEqual(errors, []);
        deepEqual(module.staticImports.map(it => it.moduleRequest.value), []);
        deepEqual(module.dynamicImports, []);
    });

    it('should define the solver, with its editors', async function () {
        // a module of no types: its components are known by their tags
        const bundle = '../dist/bundle/solver.js';
        await import(/* @vite-ignore */ bundle);
        ok(customElements.get('arg2p-solver'));
        const solver = document.createElement('arg2p-solver') as HTMLElement & { updateComplete: Promise<boolean> };
        solver.setAttribute('theory-tab', '');
        document.body.appendChild(solver);
        await solver.updateComplete;
        ok(solver.shadowRoot!.querySelector('arg2p-editor#query'));
        ok(solver.shadowRoot!.querySelector('arg2p-editor#theory'));
        solver.remove();
    });
});
