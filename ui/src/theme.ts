import { css, unsafeCSS } from 'lit';

// The look of the components, as custom properties a page sets (the dark theme of the playground, the theme of VS Code
// in a webview): each falls back to the light theme. The colours of the labels (in, out, und) stay the same in every
// theme: they carry meaning, and white is written on them (at least 4.5:1, as WCAG AA asks), dark on the yellow of no.
// What is written on the accent, or on the error, takes the colour of the surface, which they stand out on.

// The tokens, by name (--arg2p-<name> for the page, --_<name> in the components), and their values in the light theme
const LIGHT = {
    surface: '#ffffff',
    'surface-2': '#f7f7f8',
    hover: '#f4fafa',
    border: '#e3e3e3',
    text: '#212529',
    muted: '#777777',
    accent: '#008080',
    in: '#008080',
    out: '#800000',
    und: '#757575',
    unlabelled: '#2671d9',
    support: '#9a9a9a',
    no: '#c9a400',
    error: '#b00020',
    atom: '#800000',
    functor: '#6a1b9a',
    variable: 'var(--_text)',
    comment: '#808080',
    // the tones of the marks of an editor (see EditorMark): what a page links, told apart by colour
    'tone-0': '#2671d9',
    'tone-1': '#e6862a',
    'tone-2': '#2ca02c',
    'tone-3': '#c2185b',
    'tone-4': '#8a5cc2',
    'tone-5': '#8c6d1f',
    'tone-6': '#17a2b8',
    'tone-7': '#d4a017',
    font: 'system-ui, sans-serif',
    mono: 'ui-monospace, monospace',
} as const;

export type Token = keyof typeof LIGHT;

export const tokens = css`
    :host {
        ${unsafeCSS(Object.entries(LIGHT).map(([name, value]) => `--_${name}: var(--arg2p-${name}, ${value});`).join('\n'))}
        color: var(--_text);
        font-family: var(--_font);
    }
`;

/** The colour of [label] (in, out, und; any other is unlabelled) as [element] is styled, for what draws on a canvas */
export function labelColor(element: Element, label: string | null | undefined): string {
    return cssColor(element, label === 'in' || label === 'out' || label === 'und' ? label : 'unlabelled');
}

/** The value of the token [name] as [element], a component, is styled: that of the light theme where none is computed */
export function cssColor(element: Element, name: Token): string {
    const value = getComputedStyle(element).getPropertyValue(`--_${name}`).trim();
    return value !== '' && !value.startsWith('var(') ? value : LIGHT[name];
}
