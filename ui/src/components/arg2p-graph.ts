import type { Core, StylesheetJson } from 'cytoscape';
import { css, html, nothing, type PropertyValues } from 'lit';
import { EMPTY_GRAPH, type ArgumentData, type GraphData, type Solved } from '../engine.js';
import { emit } from '../events.js';
import { Canvas, nodeSize, type Drawn } from '../graph/canvas.js';
import { attackId, attacksAround, curveOf, isAbstract, isLinked, kindOf, nameOf, nodeLabel, passagesOf, statementOf } from '../graph/drawing.js';
import { layeredLayout, type Link } from '../graph/layout.js';
import { levelsOf, supportsOf } from '../graph/levels.js';
import type { Positions } from '../graph/positions.js';
import { tokens } from '../theme.js';
import { CanvasElement, canvasStyles } from './canvas-element.js';
import { popup, popupStyles, type ArgumentShown, type Named } from './popup.js';
import { buttons, chips, define } from './shared.js';

// The layout keeps nodes this far apart, edge to edge, and rows at least LEVEL_SEPARATION apart
const NODE_GAP = 40;
const LEVEL_SEPARATION = 100;

// Laid out in rows, attacks curve by their data, so that mutual ones do not overlap; supports are dashed
const STYLE: StylesheetJson = [
    {
        selector: 'edge.layered',
        style: { 'curve-style': 'unbundled-bezier', 'control-point-distances': 'data(curve)', 'control-point-weights': 0.5 },
    },
    {
        selector: 'edge.support',
        style: { 'curve-style': 'straight', 'line-style': 'dashed', 'line-dash-pattern': [3, 4], width: 1.5, 'arrow-scale': 0.7 },
    },
    { selector: 'edge.support.hidden', style: { display: 'none' } },
];

/**
 * The argument graph of a solution: every argument a node in the colour of its label, built on the arguments below it
 * (facts at the bottom) and attacking others. Hovering an argument (or tapping it, the only way on touch screens)
 * tells about it; the stages of the computation can be browsed back through the run of the solution; the graph can be
 * saved as a picture.
 *
 * An abstract framework (profile abstract) has no structure: its arguments go by the names the theory gives them and
 * are placed by their attacks alone, or where its drawing has them (see positions).
 *
 * Events: `arg2p-toggle` when supports or conclusions are shown or hidden (to remember it); `arg2p-positions` with
 * where the arguments of an abstract framework are, as they are laid out or dragged; `arg2p-export` with the picture
 * (cancel it to save the picture another way; otherwise it is downloaded); `arg2p-argument-select` {id, statement}
 * with the argument tapped and the statement of the theory it comes from (see statementOf), {id: null, statement:
 * null} when a tap is off the arguments.
 */
export class Arg2pGraph extends CanvasElement {
    static override properties = {
        solution: { attribute: false },
        positions: { attribute: false },
        showSupports: { type: Boolean, attribute: 'show-supports', reflect: true },
        showConclusions: { type: Boolean, attribute: 'show-conclusions', reflect: true },
        linked: { attribute: false },
        passages: { attribute: false },
        fileName: { attribute: 'file-name' },
        stage: { state: true },
        drawn: { state: true },
    };

    /** The solution whose graph is shown, and the run it comes from (to browse the stages of its computation) */
    solution: Solved | null = null;
    /** Where the arguments of an abstract framework are, by name, as its drawing has them (see arg2p-positions) */
    positions: Positions | null = null;
    showSupports = true;
    showConclusions = false;
    /**
     * The statements of the theory (rule names, premise names, abstract arguments) the page links to what it picked:
     * the arguments they build are outlined. A transposed rule (`r1_i`) counts as the rule it comes from.
     */
    linked: readonly string[] = [];
    /** The passages of a text each statement formalizes: shown in the popups of its arguments */
    passages: Readonly<Record<string, readonly string[]>> = {};
    /** The name of the picture the graph is saved as, with no extension */
    fileName = 'graph';

    // the stage shown, and the graph drawn: that of the solution, or of one of its stages (the stage it is of apart:
    // the stage shown is the one asked for, until its graph comes)
    private stage = 0;
    private drawn: GraphData = EMPTY_GRAPH;
    private drawnStage = 0;

    // One drawing per element: moving between the stages updates it in place, so that arguments keep their place
    protected readonly canvas = new Canvas(this, { style: STYLE, redraw: () => this.draw() });

    /** Whether there is nothing to show: no arguments, and no stages to browse (the attribute `empty`: a page may hide the graph) */
    get empty(): boolean {
        return this.drawn.arguments.length === 0 && this.last === null;
    }

    protected get blank(): boolean {
        return this.drawn.arguments.length === 0;
    }

    // The last stage of the computation, when its stages can be browsed
    private get last(): number | null {
        const solved = this.solution;
        return solved !== null && solved.run !== null && solved.solution.branch > 0 ? solved.solution.branch : null;
    }

    protected override willUpdate(changed: PropertyValues): void {
        if (changed.has('solution')) {
            this.stage = this.last ?? 0;
            this.drawn = this.solution?.solution.graph ?? EMPTY_GRAPH;
            this.drawnStage = this.stage;
        }
        this.toggleAttribute('empty', this.empty);
    }

    protected override updated(changed: PropertyValues): void {
        if (changed.has('positions')) this.canvas.give(this.positions);
        if (changed.has('drawn') || changed.has('showSupports') || changed.has('showConclusions')) this.draw();
        // the arguments linked change as the page hovers what they come from: outlined, and nothing else redrawn
        else if (changed.has('linked')) {
            const linked = new Set(this.linked);
            this.canvas.mark('linked', statement => isLinked(statement, linked));
        }
    }

    private draw(): void {
        const graph = this.drawn;
        const abstract = isAbstract(graph);
        const labels = new Map(graph.arguments.map(it => [it.id, it.label]));
        const supports = abstract ? [] : supportsOf(graph.arguments);
        const linked = new Set(this.linked);
        const nodes: Drawn[] = graph.arguments.map(it => {
            const text = nodeLabel(it, abstract, this.showConclusions);
            const statement = statementOf(it, abstract);
            return {
                group: 'nodes',
                data: { id: it.id, name: nameOf(it, abstract), statement, text, tone: it.label, ...nodeSize(text) },
                classes: isLinked(statement, linked) ? 'linked' : '',
            };
        });
        const edges: Drawn[] = [
            ...graph.attacks.map(it => ({
                group: 'edges' as const,
                data: { id: attackId(it.from, it.to), source: it.from, target: it.to, tone: labels.get(it.from) ?? 'na', curve: 0 },
                classes: abstract ? 'attack' : 'attack layered',
            })),
            ...supports.map(it => ({
                group: 'edges' as const,
                data: { id: `${it.from}=>${it.to}`, source: it.from, target: it.to, tone: 'support' },
                classes: this.showSupports ? 'support' : 'support hidden',
            })),
        ];
        // an abstract framework has no rows: its arguments are placed by their attacks, as forces, from where they are
        this.canvas.draw([...nodes, ...edges], abstract ? undefined : cy => this.layer(cy, graph, supports));
    }

    // Computed again for every graph drawn: the same arguments get the same place, so stepping through the stages does
    // not move them around
    private layer(cy: Core, graph: GraphData, supports: Link[]): void {
        if (graph.arguments.length === 0) return;
        const area = this.renderRoot.querySelector<HTMLElement>('.area')!;
        const sizes = new Map(cy.nodes().map(it => [it.id(), { width: it.data('width') as number, height: it.data('height') as number }]));
        const positions = layeredLayout(levelsOf(graph.arguments), supports, {
            sizes,
            gap: NODE_GAP,
            levelSeparation: LEVEL_SEPARATION,
            attacks: graph.attacks,
            // filling the area rather than drawing a thin strip in its middle
            aspect: area.clientWidth > 0 ? area.clientHeight / area.clientWidth : null,
        });
        cy.batch(() => {
            positions.forEach((point, id) => cy.getElementById(id).position({ x: point.x, y: point.y }));
            // attacks bend by a share of their length (a smaller one for long attacks, or they would arc across the
            // whole drawing): mutual attacks bend to either side
            graph.attacks.forEach(it => {
                const from = positions.get(it.from);
                const to = positions.get(it.to);
                if (from === undefined || to === undefined) return;
                cy.getElementById(attackId(it.from, it.to)).data('curve', curveOf(from, to) * Math.hypot(to.x - from.x, to.y - from.y));
            });
        });
    }

    // The graph of a stage comes from the engine: a later click wins, and a stage that cannot be read (the run was
    // stopped, say) leaves the stage drawn shown
    private goTo(target: number): void {
        const solved = this.solution;
        const last = this.last;
        if (solved === null || last === null || target < 0 || target > last) return;
        this.stage = target;
        const current = () => this.stage === target && this.solution === solved;
        if (target === last) return this.drawStage(solved.solution.graph, target);
        solved.run!.graphAt(target).then(
            graph => {
                if (current()) this.drawStage(graph, target);
            },
            () => {
                if (current()) this.stage = this.drawnStage;
            },
        );
    }

    private drawStage(graph: GraphData, stage: number): void {
        this.drawn = graph;
        this.drawnStage = stage;
    }

    private toggle(which: 'showSupports' | 'showConclusions', on: boolean): void {
        this[which] = on;
        emit(this, 'arg2p-toggle', { showSupports: this.showSupports, showConclusions: this.showConclusions });
    }

    protected override render() {
        const abstract = isAbstract(this.drawn);
        const last = this.last;
        return html`
            <div class="bar" part="bar">
                <div class="legend">
                    <span class="item"><span class="line attack"></span>attack</span>
                    ${abstract
                        ? nothing
                        : html`
                              <label class="item toggle" title="Show which sub-arguments every argument is built on">
                                  <input type="checkbox" .checked=${this.showSupports} @change=${(e: Event) => this.toggle('showSupports', (e.target as HTMLInputElement).checked)} />
                                  <span class="line support"></span>supports
                              </label>
                              <label class="item toggle" title="Write the conclusion of every argument under its name">
                                  <input type="checkbox" .checked=${this.showConclusions} @change=${(e: Event) => this.toggle('showConclusions', (e.target as HTMLInputElement).checked)} />
                                  conclusions
                              </label>
                          `}
                </div>
                ${this.renderPng('graph')}
                ${last === null
                    ? nothing
                    : html`
                          <div class="stages">
                              <button type="button" class="previous" title="Previous stage of the computation" aria-label="Previous stage" ?disabled=${this.stage <= 0} @click=${() => this.goTo(this.stage - 1)}>&#9664;</button>
                              <span class="stage" title="Every step of the computation keeps its own context: browse them to see how the graph is built">Stage ${this.stage} / ${last}</span>
                              <button type="button" class="next" title="Next stage of the computation" aria-label="Next stage" ?disabled=${this.stage >= last} @click=${() => this.goTo(this.stage + 1)}>&#9654;</button>
                          </div>
                      `}
            </div>
            <div class="area">
                <div class="canvas"></div>
                ${this.drawn.arguments.length === 0 && last !== null ? html`<div class="none">No arguments at this stage.</div>` : nothing}
                ${this.renderPopup(abstract)}
            </div>
        `;
    }

    private renderPopup(abstract: boolean) {
        const graph = this.drawn;
        const argument = graph.arguments.find(it => it.id === this.canvas.shown);
        if (argument === undefined) return nothing;
        const named = (id: string): Named => {
            const other: ArgumentData | undefined = graph.arguments.find(it => it.id === id);
            return { name: other === undefined ? id : nameOf(other, abstract), label: other?.label ?? 'na' };
        };
        const { by, on } = attacksAround(graph, argument.id);
        // an attack hits its target, or a sub-argument of it, told apart
        const attack = (other: string, type: string | null, target: string, hit: string | null) => ({
            other: named(other),
            type: abstract ? null : type,
            on: hit === null || hit === target ? null : named(hit),
        });
        const kind = kindOf(argument, abstract);
        const shown: ArgumentShown = {
            ...named(argument.id),
            status: argument.label === 'na' ? 'not labelled yet' : argument.label,
            kind: kind === 'assumption' || kind === 'premise' ? kind : null,
            conclusion: abstract ? null : argument.conclusion,
            topRule: kind === 'rule' ? argument.topRule : null,
            builtOn: abstract ? null : argument.supports.map(named),
            attackedBy: by.map(it => attack(it.from, it.type, it.to, it.on)),
            attacks: on.map(it => attack(it.to, it.type, it.to, it.on)),
            passages: passagesOf(this.passages, statementOf(argument, abstract)),
        };
        return popup(shown);
    }

    static override styles = [
        tokens,
        buttons,
        chips,
        popupStyles,
        canvasStyles,
        css`
            .bar {
                gap: 6px 16px;
                color: var(--_muted);
            }
            .legend {
                display: flex;
                align-items: center;
                gap: 14px;
                margin-right: auto;
            }
            .item {
                display: inline-flex;
                align-items: center;
                gap: 6px;
            }
            .toggle {
                cursor: pointer;
                user-select: none;
            }
            /* attacks take the colour of the attacker: the legend shows the stroke alone */
            .line {
                display: inline-block;
                width: 26px;
                border-top: 2px solid var(--_muted);
            }
            .line.support {
                border-top: 2px dashed var(--_support);
            }
            .stages {
                display: flex;
                align-items: center;
                gap: 8px;
            }
            .stage {
                min-width: 80px;
                text-align: center;
                font-variant-numeric: tabular-nums;
            }
            .none {
                position: absolute;
                top: 40%;
                width: 100%;
                text-align: center;
                color: var(--_muted);
                font-style: italic;
                pointer-events: none;
            }
        `,
    ];
}

define('arg2p-graph', Arg2pGraph);

declare global {
    interface HTMLElementTagNameMap {
        'arg2p-graph': Arg2pGraph;
    }
}
