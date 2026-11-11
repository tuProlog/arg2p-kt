import { css, html, nothing } from 'lit';
import { chip } from './shared.js';

// What a drawing tells about an argument hovered or tapped: the graph of a solution and the drawing of a framework
// tell it alike, each with what it knows.

/** An argument, by its name and its label (in, out, und, na) */
export interface Named {
    readonly name: string;
    readonly label: string;
}

/** An attack on or by an argument: the other argument, its type, and the sub-argument it hits when another one */
export interface AttackShown {
    readonly other: Named;
    readonly type: string | null;
    readonly on: Named | null;
}

export interface ArgumentShown extends Named {
    /** What its label means here: the label, or why there is none */
    readonly status: string;
    /** What kind of argument it is, when that is worth telling (a premise, an assumption, derived) */
    readonly kind: string | null;
    readonly conclusion: string | null;
    readonly topRule: string | null;
    /** The arguments it is built on, for a structured argument */
    readonly builtOn: readonly Named[] | null;
    readonly attackedBy: readonly AttackShown[];
    readonly attacks: readonly AttackShown[];
    /** The passages of a text its statement formalizes, when the page links them (see `passages`) */
    readonly passages?: readonly string[];
}

/** The popup of [argument], placed next to its node by the canvas */
export function popup(argument: ArgumentShown) {
    const named = (it: Named) => chip(it.name, it.label);
    const attack = (it: AttackShown) => html`
        <span class="attack">${named(it.other)}${it.type === null ? nothing : html`<span class="type">${it.type}${it.on === null ? nothing : html` on ${named(it.on)}`}</span>`}</span>
    `;
    return html`
        <div class="popup" part="popup" role="tooltip">
            <div class="header">
                ${named(argument)}
                <span class="label">${argument.status}</span>
                ${argument.kind === null ? nothing : html`<span class="kind">${argument.kind}</span>`}
            </div>
            ${argument.conclusion === null ? nothing : row('Conclusion', html`<div class="term">${argument.conclusion}</div>`)}
            ${argument.topRule === null ? nothing : row('Top rule', html`<div class="term">${argument.topRule}</div>`)}
            ${argument.builtOn === null ? nothing : row('Built on', chips(argument.builtOn.map(named)))}
            ${row('Attacked by', chips(argument.attackedBy.map(attack)))}
            ${row('Attacks', chips(argument.attacks.map(attack)))}
            ${argument.passages === undefined || argument.passages.length === 0
                ? nothing
                : row('From the text', html`${argument.passages.map(it => html`<blockquote class="passage">${it}</blockquote>`)}`)}
        </div>
    `;
}

function row(title: string, content: unknown) {
    return html`<div class="row"><div class="title">${title}</div>${content}</div>`;
}

function chips(items: unknown[]) {
    return html`<div class="chips">${items.length === 0 ? html`<span class="muted">none</span>` : items}</div>`;
}

export const popupStyles = css`
    .popup .passage {
        margin: 2px 0 0 0;
        padding: 2px 0 2px 8px;
        border-left: 3px solid var(--_accent);
        font-style: italic;
        line-height: 1.4;
    }
    .popup {
        position: absolute;
        z-index: 20;
        width: max-content;
        max-width: min(320px, calc(100% - 16px));
        padding: 10px 12px;
        background: var(--_surface);
        border: 1px solid var(--_border);
        border-radius: 8px;
        box-shadow: 0 4px 14px rgba(0, 0, 0, 0.15);
        pointer-events: none;
    }
    .popup .header {
        display: flex;
        align-items: center;
        gap: 8px;
        margin-bottom: 6px;
    }
    .popup .label,
    .popup .kind {
        font-size: 11px;
        font-weight: bold;
        text-transform: uppercase;
        letter-spacing: 0.04em;
        color: var(--_muted);
    }
    .popup .kind {
        font-weight: normal;
    }
    .popup .row {
        display: grid;
        grid-template-columns: 82px 1fr;
        column-gap: 8px;
        align-items: baseline;
        padding: 3px 0;
    }
    .popup .title,
    .popup .muted,
    .popup .type {
        color: var(--_muted);
    }
    .popup .term {
        font-family: var(--_mono);
        word-break: break-word;
    }
    .popup .chips {
        display: flex;
        flex-wrap: wrap;
        gap: 4px;
    }
    .popup .attack {
        display: inline-flex;
        align-items: center;
        gap: 4px;
    }
    .popup .type,
    .popup .chip {
        font-size: 12px;
    }
`;
