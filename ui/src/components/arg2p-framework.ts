import type { Core, EventObject, StylesheetJson } from 'cytoscape';
import { css, html, nothing, type PropertyValues } from 'lit';
import type { GraphData } from '../engine.js';
import { emit } from '../events.js';
import { isAtom } from '../formats/atoms.js';
import { declaredFramework, redrawn } from '../formats/declarations.js';
import { frameworkOf, type Framework } from '../formats/frameworks.js';
import { Canvas, nodeSize, type Drawn } from '../graph/canvas.js';
import { attackId, isAbstract, nameOf } from '../graph/drawing.js';
import type { Positions } from '../graph/positions.js';
import { tokens } from '../theme.js';
import { CanvasElement, canvasStyles } from './canvas-element.js';
import { popup, popupStyles, type AttackShown } from './popup.js';
import { buttons, chips, define } from './shared.js';

type Mode = 'select' | 'argument' | 'attack';

/** An argument or an attack drawn, and whether the theory derives it without declaring it */
interface Shown<T> {
    readonly it: T;
    readonly derived: boolean;
}

const STYLE: StylesheetJson = [
    // the attacker picked, in attack mode
    { selector: 'node.source', style: { 'border-width': 3, 'border-color': 'data(outline)' } },
    { selector: 'edge:selected', style: { width: 4 } },
    // what the theory derives and does not declare: drawn, not edited by drawing
    { selector: 'node.derived', style: { 'border-width': 3, 'border-style': 'dashed', 'border-color': 'data(outline)', 'background-opacity': 0.8 } },
    {
        selector: 'edge.derived',
        style: { 'line-style': 'dashed', 'line-dash-pattern': [9, 6], width: 2.5, 'arrow-scale': 1.4 },
    },
];

/**
 * An abstract framework drawn: the arguments and attacks its theory (of the profile abstract) declares, as nodes and
 * arrows. Drawing writes the theory: an argument added is a fact `a.`, an attack `a -> b.`; deleting or renaming one
 * changes its declaration, and every other clause of the theory (those that derive attacks, say) stays as it is.
 * The theory edited as text is drawn again, the arguments kept where they were.
 *
 * Given the graph of a solution, the arguments take the colours of their labels, and what the theory derives without
 * declaring it is drawn too, dashed: it changes with the clauses that derive it, not by drawing. Hovering an argument
 * (or tapping it) tells about it.
 *
 * Events: `arg2p-theory-change` with the theory, after every change made by drawing; `arg2p-positions` with where the
 * arguments are, as they are laid out or dragged; `arg2p-export` with the picture of the framework, as for the graph;
 * `arg2p-argument-select` {id, statement} with the argument tapped while selecting (its name, both), as for the graph.
 */
export class Arg2pFramework extends CanvasElement {
    static override properties = {
        theory: {},
        graph: { attribute: false },
        positions: { attribute: false },
        readonly: { type: Boolean, reflect: true },
        linked: { attribute: false },
        passages: { attribute: false },
        fileName: { attribute: 'file-name' },
        mode: { state: true },
        selected: { state: true },
        renaming: { state: true },
        source: { state: true },
    };

    /** The theory drawn */
    theory = '';
    /** The graph of a solution of [theory], to show its labels and what it derives; null when there is none */
    graph: GraphData | null = null;
    /** Where the arguments are, by name, as the graph of the framework has them (see arg2p-positions) */
    positions: Positions | null = null;
    readonly = false;
    /** The arguments the page links to what it picked: they are outlined */
    linked: readonly string[] = [];
    /** The passages of a text each argument formalizes: shown in its popup */
    passages: Readonly<Record<string, readonly string[]>> = {};
    /** The name of the picture the framework is saved as, with no extension */
    fileName = 'framework';

    private mode: Mode = 'select';
    private selected = false;
    private renaming: { name: string; value: string } | null = null;
    // the attacker picked, in attack mode
    private source: string | null = null;

    // The graph of the theory drawn (that of another theory has labels that no longer hold), and what is drawn: the
    // framework the theory declares, and what the graph adds to it
    private labelled: GraphData | null = null;
    private declared: Framework = { arguments: [], attacks: [] };
    private drawnArguments: readonly Shown<string>[] = [];
    private drawnAttacks: readonly Shown<{ from: string; to: string }>[] = [];
    private labels: ReadonlyMap<string, string> = new Map();

    protected readonly canvas = new Canvas(this, {
        style: STYLE,
        redraw: () => this.draw(),
        setup: cy => this.listen(cy),
        // read only, a tap still tells about an argument
        pins: () => this.readonly || this.mode === 'select',
        locked: () => this.readonly,
    });

    protected get blank(): boolean {
        return this.drawnArguments.length === 0;
    }

    protected override willUpdate(changed: PropertyValues): void {
        if (changed.has('graph')) this.labelled = this.graph !== null && isAbstract(this.graph) ? this.graph : null;
        else if (changed.has('theory')) this.labelled = null;
        if (changed.has('theory') || changed.has('graph')) this.read();
        if (changed.has('readonly') && this.readonly) this.setMode('select');
    }

    protected override updated(changed: PropertyValues): void {
        if (changed.has('positions')) this.canvas.give(this.positions);
        if (changed.has('theory') || changed.has('graph')) this.draw();
        // the arguments linked change as the page hovers what they come from: outlined, and nothing else redrawn
        else if (changed.has('linked')) {
            const linked = new Set(this.linked);
            this.canvas.mark('linked', name => linked.has(name));
        }
        if (changed.has('renaming') && this.renaming !== null && changed.get('renaming') === null) {
            const box = this.renderRoot.querySelector<HTMLInputElement>('.rename');
            this.placeRename();
            box?.focus();
            box?.select();
        }
    }

    // The declared framework, and what the graph adds to it
    private read(): void {
        this.declared = declaredFramework(this.theory);
        const solved = this.labelled === null ? { arguments: [], attacks: [] } : frameworkOf(this.labelled);
        const declaredAttacks = new Set(this.declared.attacks.map(it => attackId(it.from, it.to)));
        this.drawnArguments = [
            ...this.declared.arguments.map(it => ({ it, derived: false })),
            ...solved.arguments.filter(it => !this.declared.arguments.includes(it)).map(it => ({ it, derived: true })),
        ];
        this.drawnAttacks = [
            ...this.declared.attacks.map(it => ({ it, derived: false })),
            ...solved.attacks.filter(it => !declaredAttacks.has(attackId(it.from, it.to))).map(it => ({ it, derived: true })),
        ];
        this.labels = new Map((this.labelled?.arguments ?? []).map(it => [nameOf(it, true), it.label]));
        if (this.source !== null && !this.drawnArguments.some(({ it }) => it === this.source)) this.source = null;
    }

    // Drawn as it was read: the arguments already drawn stay where they are, the new ones go where the graph has them,
    // or around the others
    private draw(): void {
        const tone = (name: string) => this.labels.get(name) ?? 'na';
        const linked = new Set(this.linked);
        const nodes: Drawn[] = this.drawnArguments.map(({ it, derived }) => ({
            group: 'nodes',
            data: { id: it, name: it, statement: it, text: it, tone: tone(it), ...nodeSize(it) },
            classes: [derived ? 'derived' : '', it === this.source ? 'source' : '', linked.has(it) ? 'linked' : ''].join(' ').trim(),
        }));
        const edges: Drawn[] = this.drawnAttacks.map(({ it, derived }) => ({
            group: 'edges',
            data: { id: attackId(it.from, it.to), source: it.from, target: it.to, tone: tone(it.from) },
            classes: derived ? 'derived' : '',
        }));
        this.canvas.draw([...nodes, ...edges]);
    }

    private listen(cy: Core): void {
        cy.on('tap', (event: EventObject) => this.tap(event));
        cy.on('dbltap', (event: EventObject) => {
            if (this.readonly) return;
            if (event.target === cy) this.add(event.position);
            else if (event.target.isNode()) this.startRename(event.target.id());
        });
        cy.on('select unselect', () => {
            this.selected = cy.$(':selected').nonempty();
        });
        // the rename box stays on its node while the drawing moves
        cy.on('viewport position', () => this.placeRename());
    }

    // Drawing with a tap: an argument where it is, or an attack from the attacker picked
    private tap(event: EventObject): void {
        const cy = event.cy;
        if (this.readonly) return;
        if (this.mode === 'argument' && event.target === cy) this.add(event.position);
        else if (this.mode === 'attack') {
            if (event.target === cy || !event.target.isNode()) return this.pick(null);
            const id = event.target.id() as string;
            if (this.source === null) return this.pick(id);
            const from = this.source;
            this.pick(null);
            const known = this.declared.arguments.includes(from) && this.declared.arguments.includes(id);
            if (known && !this.declared.attacks.some(it => it.from === from && it.to === id)) {
                this.change({ arguments: this.declared.arguments, attacks: [...this.declared.attacks, { from, to: id }] });
            }
        }
    }

    private pick(id: string | null): void {
        this.canvas.cy?.$('.source').removeClass('source');
        this.source = id;
        if (id !== null) this.canvas.cy?.getElementById(id).addClass('source');
    }

    private add(position: { x: number; y: number }): void {
        const name = this.suggestedName();
        // placed where it was asked for before the theory draws it
        this.canvas.add({ group: 'nodes', data: { id: name, name, text: name, tone: 'na', ...nodeSize(name) }, classes: '' }, position);
        this.change({ arguments: [...this.declared.arguments, name], attacks: this.declared.attacks });
    }

    // A change made by drawing: the theory rewritten, then drawn again from it
    private change(framework: Framework, renamed: ReadonlyMap<string, string> = new Map()): void {
        this.theory = redrawn(this.theory, framework, renamed);
        emit(this, 'arg2p-theory-change', { theory: this.theory });
    }

    // Only what is declared is deleted by drawing
    private deleteSelected(): void {
        const cy = this.canvas.cy;
        if (cy === null) return;
        const gone = new Set(cy.$('node:selected').map(it => it.id()).filter(it => this.declared.arguments.includes(it)));
        const goneAttacks = new Set(cy.$('edge:selected').map(it => it.id()));
        this.change({
            arguments: this.declared.arguments.filter(it => !gone.has(it)),
            attacks: this.declared.attacks.filter(it => !gone.has(it.from) && !gone.has(it.to) && !goneAttacks.has(attackId(it.from, it.to))),
        });
        this.selected = cy.$(':selected').nonempty();
    }

    private setMode(mode: Mode): void {
        this.mode = mode;
        this.pick(null);
    }

    private suggestedName(): string {
        const taken = new Set(this.canvas.cy?.nodes().map(it => it.id()) ?? []);
        for (let i = 1; ; i++) if (!taken.has(`a${i}`)) return `a${i}`;
    }

    private isAvailable(name: string, current: string): boolean {
        return isAtom(name) && (name === current || (this.canvas.cy?.getElementById(name).empty() ?? true));
    }

    private startRename(name: string): void {
        if (this.declared.arguments.includes(name)) this.renaming = { name, value: name };
    }

    // An invalid or taken name is dropped: the argument keeps its name
    private commitRename(): void {
        const renaming = this.renaming;
        this.renaming = null;
        if (renaming === null) return;
        const wanted = renaming.value.trim();
        if (wanted === renaming.name || !this.isAvailable(wanted, renaming.name)) return;
        const node = this.canvas.cy?.getElementById(renaming.name);
        // the renamed argument stays where it was
        if (node?.nonempty()) {
            this.canvas.add({ group: 'nodes', data: { ...node.data(), id: wanted, name: wanted, text: wanted, ...nodeSize(wanted) }, classes: '' }, node.position());
        }
        const rename = (it: string) => (it === renaming.name ? wanted : it);
        this.change(
            {
                arguments: this.declared.arguments.map(rename),
                attacks: this.declared.attacks.map(it => ({ from: rename(it.from), to: rename(it.to) })),
            },
            new Map([[renaming.name, wanted]]),
        );
    }

    private placeRename(): void {
        const box = this.renderRoot.querySelector<HTMLElement>('.rename');
        const node = this.renaming === null ? null : this.canvas.cy?.getElementById(this.renaming.name);
        if (box === null || node == null || node.empty()) return;
        const point = node.renderedPosition();
        box.style.left = `${point.x}px`;
        box.style.top = `${point.y}px`;
    }

    private onKey(event: KeyboardEvent): void {
        if (this.readonly || this.renaming !== null) return;
        if (event.key === 'Escape') this.setMode('select');
        else if ((event.key === 'Delete' || event.key === 'Backspace') && this.selected) {
            event.preventDefault();
            this.deleteSelected();
        }
    }

    protected override render() {
        const tool = (mode: Mode, text: string, title: string) => html`
            <button type="button" class=${this.mode === mode ? 'active' : ''} aria-pressed=${this.mode === mode} title=${title} @click=${() => this.setMode(mode)}>${text}</button>
        `;
        const renaming = this.renaming;
        const hint =
            this.mode === 'argument' ? 'Click where an argument goes'
            : this.mode === 'attack' ? (this.source === null ? 'Click the attacker' : `Click the argument ${this.source} attacks`)
            : 'Double click to add an argument, or to rename one';
        return html`
            <div class="bar" part="bar">
                ${this.readonly
                    ? nothing
                    : html`
                          ${tool('select', 'Select', 'Select and move arguments (Esc)')}
                          ${tool('argument', '+ Argument', 'Add arguments')}
                          ${tool('attack', '+ Attack', 'Add attacks: the attacker, then the argument it attacks')}
                          <button type="button" title="Delete what is selected (Del)" ?disabled=${!this.selected} @click=${() => this.deleteSelected()}>Delete</button>
                          <span class="hint">${hint}</span>
                      `}
                ${this.renderPng('framework')}
            </div>
            <div
                class="area"
                tabindex="0"
                role="group"
                aria-label=${this.readonly ? 'The framework drawn' : 'The framework drawn: Delete removes what is selected, Escape goes back to selecting'}
                @keydown=${(e: KeyboardEvent) => this.onKey(e)}
            >
                <div class="canvas"></div>
                ${this.renderPopup()}
                ${this.drawnArguments.some(it => it.derived) || this.drawnAttacks.some(it => it.derived)
                    ? html`<div class="legend" part="legend">
                          <span><span class="line"></span>declared in the theory: drawing changes it</span>
                          <span><span class="line derived"></span>derived by its rules: change the rules to change it</span>
                      </div>`
                    : nothing}
                ${renaming === null
                    ? nothing
                    : html`<input
                          class="rename ${this.isAvailable(renaming.value.trim(), renaming.name) ? '' : 'invalid'}"
                          type="text"
                          aria-label="Argument name"
                          title="Argument name: a lowercase letter, then letters, digits or underscores"
                          .value=${renaming.value}
                          @input=${(e: Event) => (this.renaming = { ...renaming, value: (e.target as HTMLInputElement).value })}
                          @keydown=${(e: KeyboardEvent) => {
                              e.stopPropagation();
                              if (e.key === 'Enter') this.commitRename();
                              else if (e.key === 'Escape') this.renaming = null;
                          }}
                          @blur=${() => this.commitRename()}
                      />`}
            </div>
        `;
    }

    // An argument: its label, and the arguments attacking it and those it attacks
    private renderPopup() {
        const shown = this.drawnArguments.find(({ it }) => it === this.canvas.shown);
        if (shown === undefined) return nothing;
        const name = shown.it;
        const named = (it: string) => ({ name: it, label: this.labels.get(it) ?? 'na' });
        const attack = (other: string): AttackShown => ({ other: named(other), type: null, on: null });
        const label = named(name).label;
        return popup({
            ...named(name),
            status: this.labelled === null ? 'not solved yet' : label === 'na' ? 'not labelled yet' : label,
            kind: shown.derived ? 'derived' : null,
            conclusion: null,
            topRule: null,
            builtOn: null,
            attackedBy: this.drawnAttacks.filter(({ it }) => it.to === name).map(({ it }) => attack(it.from)),
            attacks: this.drawnAttacks.filter(({ it }) => it.from === name).map(({ it }) => attack(it.to)),
            passages: this.passages[name] ?? [],
        });
    }

    static override styles = [
        tokens,
        buttons,
        chips,
        popupStyles,
        canvasStyles,
        css`
            .bar {
                gap: 6px;
            }
            button.active {
                border-color: var(--_accent);
                color: var(--_accent);
            }
            .hint {
                margin-left: auto;
                color: var(--_muted);
            }
            .png {
                margin-left: auto;
            }
            .hint + .png {
                margin-left: 0;
            }
            .area:focus {
                outline: none;
            }
            .area:focus-visible {
                box-shadow: inset 0 0 0 2px color-mix(in srgb, var(--_accent) 45%, transparent);
            }
            .legend {
                position: absolute;
                left: 8px;
                bottom: 8px;
                display: flex;
                flex-direction: column;
                gap: 2px;
                padding: 6px 10px;
                font-size: 12px;
                color: var(--_muted);
                background: color-mix(in srgb, var(--_surface) 88%, transparent);
                border: 1px solid var(--_border);
                border-radius: 6px;
                pointer-events: none;
            }
            .legend > span {
                display: inline-flex;
                align-items: center;
                gap: 8px;
            }
            .legend .line {
                display: inline-block;
                width: 28px;
                border-top: 2.5px solid var(--_muted);
            }
            .legend .line.derived {
                border-top-style: dashed;
            }
            .rename {
                position: absolute;
                transform: translate(-50%, -50%);
                width: 9em;
                font: 14px var(--_mono);
                text-align: center;
                color: var(--_text);
                background: var(--_surface);
                border: 2px solid var(--_accent);
                border-radius: 4px;
                padding: 2px 4px;
                z-index: 10;
            }
            .rename.invalid {
                border-color: var(--_error);
            }
        `,
    ];
}

define('arg2p-framework', Arg2pFramework);

declare global {
    interface HTMLElementTagNameMap {
        'arg2p-framework': Arg2pFramework;
    }
}
