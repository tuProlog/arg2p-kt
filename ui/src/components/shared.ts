import { css, html } from 'lit';

// What the components have in common: how they are defined (they tell what happens with emit, see events), and the
// look of their buttons and of the chips of arguments.

/** Defines [element] as [name], unless the page has it already (two copies of the components on one page) */
export function define(name: string, element: CustomElementConstructor): void {
    if (customElements.get(name) === undefined) customElements.define(name, element);
}

/** [action] on Enter or Space, for what acts as a button without being one */
export function activate(event: KeyboardEvent, action: () => void): void {
    if (event.key !== 'Enter' && event.key !== ' ') return;
    event.preventDefault();
    action();
}

/** An argument (or a statement) by its name, in the colour of its label (in, out, und; any other is unlabelled) */
export function chip(name: string, label: string) {
    return html`<span class="chip ${label}">${name}</span>`;
}

export const buttons = css`
    button {
        display: inline-flex;
        align-items: center;
        gap: 4px;
        font: inherit;
        color: var(--_text);
        background: var(--_surface);
        border: 1px solid var(--_border);
        border-radius: 4px;
        padding: 2px 8px;
        cursor: pointer;
    }
    button:hover:not(:disabled) {
        background: var(--_hover);
    }
    button:disabled {
        opacity: 0.5;
        cursor: default;
    }
`;

// The labels are written on their colours alike in every theme
export const chips = css`
    .chip {
        color: #ffffff;
        border-radius: 10px;
        padding: 0 8px;
        background: var(--_unlabelled);
    }
    .chip.in {
        background: var(--_in);
    }
    .chip.out {
        background: var(--_out);
    }
    .chip.und {
        background: var(--_und);
    }
`;
