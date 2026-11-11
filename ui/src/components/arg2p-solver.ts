import { LitElement, css, html, nothing, type PropertyValues } from 'lit';
import type { Engine, EngineError, SolveRequest, Solved } from '../engine.js';
import { emit } from '../events.js';
import { profileInfo } from '../flags/schema.js';
import { profileOf } from '../flags/text.js';
import type { Positions } from '../graph/positions.js';
import { tokens } from '../theme.js';
import './arg2p-editor.js';
import type { Arg2pEditor } from './arg2p-editor.js';
import './arg2p-flags.js';
import './arg2p-framework.js';
import type { Arg2pFramework } from './arg2p-framework.js';
import './arg2p-graph.js';
import type { Arg2pGraph } from './arg2p-graph.js';
import './arg2p-solutions.js';
import type { Arg2pSolutions, SolutionsState } from './arg2p-solutions.js';
import { buttons, define } from './shared.js';

/** The tabs of the solver itself; those of the page are named by the ids of their elements */
type Panel = 'results' | 'theory' | 'flags' | 'console';

const PANELS: readonly string[] = ['results', 'theory', 'flags', 'console'] satisfies Panel[];

/** A tab of the page: its element, by its id, and what the tab reads */
interface HostTab {
    readonly id: string;
    readonly label: string;
}

/** The query the editor suggests: the labelling of the statements */
const QUERY = 'arg2p::solve(In, Out, Und)';

/**
 * A solver of a theory: its query, run on an engine (Run, or Enter; Stop while it runs), and in tabs the solutions
 * with the graph of the one shown (for an abstract framework, its drawing, which edits the theory), the flags (as
 * controls and as text), and what the solver prints. A page without an editor of its own for the theory asks for
 * one in a tab of its own (`theory-tab`).
 *
 * The page gives the engine, the theory, the query and the flags, and is told how they change. A syntax error is
 * revealed where it is: in the query, in the flags, in the theory of the Theory tab, or else told to the page
 * (`arg2p-theory-error`), which has the theory.
 *
 * Events: `arg2p-query-change` {query}, `arg2p-flags-change` {flags}, `arg2p-theory-change` {theory} (by drawing, or in
 * the Theory tab) as they are edited; `arg2p-run` {theory, query, flags} as a run starts; `arg2p-busy` {busy};
 * `arg2p-error` {error} with the EngineError of a run that could not start, or of a solution that could not be
 * computed (stopped, say); `arg2p-theory-error` {problem} with where the theory does not parse, without a Theory tab.
 * Those of the components inside come out too: `arg2p-solution`, `arg2p-positions`, `arg2p-toggle`, `arg2p-export`,
 * `arg2p-argument-select` (`arg2p-argument-hover` as one is hovered). The arguments of the statements `linked` are outlined
 * in the graph and the drawing, and the `passages` of a text their statements formalize are shown in their popups.
 *
 * The solutions can be folded away, for the graph to take their room (`solutions-collapsed`; `arg2p-solutions-toggle`
 * {collapsed} as the user folds or unfolds them).
 *
 * Slots: `actions`, at the end of the bar of the tabs (links, full screen, ...); `split`, between the solutions and
 * the graph (a handle that resizes them, setting --arg2p-solutions-width); `tab`, the tabs of the page: each element
 * in it is a tab after those of the solver, named by its `id` and reading its `label` attribute, shown when picked.
 * The tab shown is `tab` (`results`, `flags`, `console`, `theory`, or the id of a tab of the page), and
 * `arg2p-tab-change` {tab} tells another one picked.
 */
/** What a solver shows, saved to be shown again (see save and restore) */
export interface SolverState {
    readonly solutions: SolutionsState;
    readonly solved: Solved | null;
    readonly output: string;
    /** The run the output comes from, which goes on printing there */
    readonly printing: object | null;
    readonly notice: string | null;
    readonly positions: Positions | null;
}

export class Arg2pSolver extends LitElement {
    static override properties = {
        engine: { attribute: false },
        theory: {},
        query: {},
        flags: {},
        fileName: { attribute: 'file-name' },
        theoryTab: { type: Boolean, attribute: 'theory-tab' },
        linked: { attribute: false },
        passages: { attribute: false },
        solutionsCollapsed: { type: Boolean, attribute: 'solutions-collapsed', reflect: true },
        tab: {},
        hosted: { state: true },
        solved: { state: true },
        output: { state: true },
        notice: { state: true },
        positions: { state: true },
        starting: { state: true },
        computing: { state: true },
    };

    /** What solves, and checks the texts */
    engine: Engine | null = null;
    theory = '';
    query = '';
    /** The flags, as Prolog facts */
    flags = '';
    /** The name the pictures of the graph and the drawing are saved as, with no extension */
    fileName = 'theory';
    /** Whether the theory is edited in a tab of its own */
    theoryTab = false;
    /** The statements the page links to what it picked: their arguments are outlined in the graph and the drawing */
    linked: readonly string[] = [];
    /** The passages of a text each statement formalizes: shown in the popups of its arguments, in the graph and the drawing */
    passages: Readonly<Record<string, readonly string[]>> = {};
    /** Whether the solutions are folded away, beside the graph (unfolded on narrow screens, where they are above it) */
    solutionsCollapsed = false;
    /** The tab shown: one of the solver (`results`, `flags`, `console`, `theory`), or the id of one of the page */
    tab: Panel | string = 'results';

    // the tabs of the page, as its elements in the slot `tab` are now
    private hosted: readonly HostTab[] = [];
    private readonly observer = new MutationObserver(() => this.adopt());
    // the solution shown, what the run printed, and what a failure tells
    private solved: Solved | null = null;
    private output = '';
    private notice: string | null = null;
    // where the arguments of an abstract framework are, shared by its graph and its drawing
    private positions: Positions | null = null;
    // a run being started, a solution being computed: what is told as busy
    private starting = false;
    private computing = false;
    private told = false;
    // the runs started: what a run no longer the latest answers is let go
    private runs = 0;
    // the run whose output is shown (it goes with its results, when they are saved and restored)
    private printing: object | null = null;
    // the run whose first solution the drawing was fitted to, and whether an abstract framework was shown last
    private fitted = 0;
    private wasAbstract = false;

    /** Whether a run is being started, or a solution computed (the attribute `busy`) */
    get busy(): boolean {
        return this.starting || this.computing;
    }

    /** Solves the query: the results of the run before are let go, and it is stopped if it was still running */
    async run(): Promise<void> {
        const engine = this.engine;
        if (engine === null) return;
        this.reset();
        const run = this.runs;
        const request: SolveRequest = { theory: this.theory, query: this.query, flags: this.flags };
        const printing = {};
        this.printing = printing;
        this.select('results');
        this.starting = true;
        emit(this, 'arg2p-run', request);
        await this.updateComplete;
        if (run !== this.runs) return;
        // (not rendered yet: nothing to run into)
        const solutions = this.find<Arg2pSolutions>('arg2p-solutions');
        if (!solutions) return;
        try {
            const started = await engine.solve(request, text => {
                if (printing === this.printing) this.output += text;
            });
            if (run !== this.runs) return;
            // busy all along: the first solution is asked for before the start is over
            const first = solutions.start(started);
            this.starting = false;
            await first;
        } catch (error) {
            if (run !== this.runs) return;
            this.starting = false;
            const failure = error as EngineError;
            solutions.fail(failure);
            this.reveal(failure);
            emit(this, 'arg2p-error', { error: failure });
        }
    }

    /** Stops the run under way: what it was computing fails, as stopped */
    stop(): void {
        if (this.busy) this.engine?.stop();
    }

    /**
     * What the solver shows now (its solutions, the one shown, what the run printed, where the arguments are), to show
     * it again with restore: a page with several theories keeps the results of each
     */
    save(): SolverState {
        return {
            solutions: this.find<Arg2pSolutions>('arg2p-solutions')?.save() ?? { run: null, entries: [], error: null, shown: -1 },
            solved: this.solved,
            output: this.output,
            printing: this.printing,
            notice: this.notice,
            positions: this.positions,
        };
    }

    /**
     * Shows [state] again, as saved, or nothing (null): a run under way is stopped first. Its run goes on from where it
     * was, on the engine the solver has then.
     */
    restore(state: SolverState | null): void {
        this.reset();
        if (state === null) {
            this.positions = null;
            return;
        }
        this.find<Arg2pSolutions>('arg2p-solutions')?.restore(state.solutions);
        this.solved = state.solved;
        this.output = state.output;
        this.printing = state.printing;
        this.notice = state.notice;
        this.positions = state.positions;
        this.fitted = this.runs;
    }

    /** Forgets the results, and where the arguments were drawn (another theory is shown), stopping a run under way */
    clear(): void {
        this.reset();
        this.positions = null;
    }

    /** Takes to where [error] does not parse: the query, the flags, the theory in its tab, or else tells the page */
    reveal(error: EngineError): void {
        const syntax = error.syntax;
        if (syntax === null) return;
        if (syntax.where === 'query') return this.find<Arg2pEditor>('#query')?.report(syntax);
        if (syntax.where === 'theory' && !this.theoryTab) {
            emit(this, 'arg2p-theory-error', { problem: syntax });
            return;
        }
        this.select(syntax.where);
        this.updateComplete.then(() => {
            const details = this.find<HTMLDetailsElement>('.flags-text');
            if (syntax.where === 'flags' && details !== null) details.open = true;
            this.find<Arg2pEditor>(`#${syntax.where}`)?.report(syntax);
        });
    }

    /** Fits the graph and the drawing to their room (after the page resized it, say) */
    fit(): void {
        this.find<Arg2pGraph>('arg2p-graph')?.fit();
        this.find<Arg2pFramework>('arg2p-framework')?.fit();
    }

    /** Draws the graph and the drawing again, in the colours the page sets now (after a change of theme) */
    restyle(): void {
        this.find<Arg2pGraph>('arg2p-graph')?.restyle();
        this.find<Arg2pFramework>('arg2p-framework')?.restyle();
    }

    override connectedCallback(): void {
        super.connectedCallback();
        this.adopt();
        this.observer.observe(this, { childList: true, subtree: true, attributes: true, attributeFilter: ['slot', 'id', 'label'] });
    }

    // Taken out of the page: a run under way is stopped, and let go
    override disconnectedCallback(): void {
        super.disconnectedCallback();
        this.observer.disconnect();
        if (this.busy) this.reset();
    }

    // The tab shown: the one asked for, if there is one, or else the solutions
    private get shown(): string {
        const builtIn = PANELS.includes(this.tab) && (this.tab !== 'theory' || this.theoryTab);
        return builtIn || this.hosted.some(it => it.id === this.tab) ? this.tab : 'results';
    }

    // The tab [tab] picked: the page is told
    private select(tab: string): void {
        if (tab === this.shown) return;
        this.tab = tab;
        emit(this, 'arg2p-tab-change', { tab });
    }

    // The tabs as WAI-ARIA has them: the arrows pick the tab before or after (around), Home and End the first and the
    // last, which is focused
    private moveTab(event: KeyboardEvent): void {
        const tabs = [...this.renderRoot.querySelectorAll<HTMLButtonElement>('[role=tab]')];
        const at = tabs.indexOf(event.target as HTMLButtonElement);
        const to: number | undefined = ({ ArrowLeft: at - 1, ArrowRight: at + 1, Home: 0, End: tabs.length - 1 } as Record<string, number>)[event.key];
        if (at === -1 || to === undefined) return;
        event.preventDefault();
        const tab = tabs[(to + tabs.length) % tabs.length];
        this.select(tab.dataset.tab!);
        tab.focus();
    }

    // The elements of the page in the slot `tab`, as tabs
    private adopt(): void {
        const hosted = this.tabElements().map(it => ({ id: it.id, label: it.getAttribute('label') ?? it.id }));
        if (JSON.stringify(hosted) !== JSON.stringify(this.hosted)) this.hosted = hosted;
    }

    private tabElements(): HTMLElement[] {
        return [...this.children].filter((it): it is HTMLElement => it instanceof HTMLElement && it.slot === 'tab' && it.id !== '');
    }

    // The profile the theory is read in, as its texts state it: the one it states, or the flags, or the default
    private get profile(): string {
        return profileOf(this.theory, this.flags);
    }

    private get abstract(): boolean {
        return this.profile === 'abstract';
    }

    private find<T extends Element>(selector: string): T | null {
        return this.renderRoot.querySelector<T>(selector);
    }

    // The results let go, and the run under way stopped: what it still answers is stale
    private reset(): void {
        const running = this.busy;
        this.runs++;
        this.starting = false;
        this.computing = false;
        this.solved = null;
        this.output = '';
        this.printing = null;
        this.notice = null;
        this.find<Arg2pSolutions>('arg2p-solutions')?.clear();
        if (running) this.engine?.stop();
    }

    // A solution shown: an abstract framework drawn first is fitted to what the solution derives too
    // The solutions folded away, or back: the page is told, to keep it
    private toggleSolutions(): void {
        this.solutionsCollapsed = !this.solutionsCollapsed;
        emit(this, 'arg2p-solutions-toggle', { collapsed: this.solutionsCollapsed });
    }

    private show(solved: Solved): void {
        this.solved = solved;
        if (!this.abstract || this.fitted === this.runs) return;
        this.fitted = this.runs;
        const drawing = this.find<Arg2pFramework>('arg2p-framework');
        this.updateComplete.then(() => drawing?.updateComplete).then(() => drawing?.fit());
    }

    private failed(error: EngineError): void {
        this.notice = error.stopped ? 'Stopped before the next solution.' : error.message;
        emit(this, 'arg2p-error', { error });
    }

    private edit(what: 'theory' | 'query' | 'flags', value: string): void {
        this[what] = value;
        emit(this, `arg2p-${what}-change`, { [what]: value });
    }

    protected override willUpdate(): void {
        this.toggleAttribute('busy', this.busy);
    }

    protected override updated(changed: PropertyValues): void {
        // the tabs of the page: the one picked shown alone, named by its label (its tab, in the solver, is out of its reach)
        const shown = this.shown;
        for (const element of this.tabElements()) {
            element.setAttribute('role', 'tabpanel');
            element.setAttribute('aria-label', element.getAttribute('label') ?? element.id);
            element.hidden = element.id !== shown;
        }
        if (this.busy !== this.told) {
            this.told = this.busy;
            emit(this, 'arg2p-busy', { busy: this.busy });
        }
        // the graph, or the drawing, fits the room it is given once shown
        if (changed.has('tab') || this.abstract !== this.wasAbstract) {
            this.wasAbstract = this.abstract;
            this.fit();
        }
    }

    protected override render() {
        const shown = this.shown;
        // a tab, and the panel it shows (those of the page are in the slot of their own)
        const tab = (panel: string, text: string) => html`
            <button
                type="button"
                role="tab"
                id="tab-${panel}"
                aria-controls=${PANELS.includes(panel) ? `panel-${panel}` : 'panel-hosted'}
                aria-selected=${shown === panel}
                tabindex=${shown === panel ? 0 : -1}
                data-tab=${panel}
                class=${shown === panel ? 'active' : ''}
                @click=${() => this.select(panel)}
            >
                ${text}
            </button>
        `;
        // what the components inside tell is told by the solver, as its own
        const own = (action: (detail: never) => void) => (event: CustomEvent) => {
            event.stopPropagation();
            action(event.detail as never);
        };
        const info = profileInfo(this.profile);
        const abstract = this.abstract;
        return html`
            <div class="query" part="query">
                <span class="prompt" aria-hidden="true">?-</span>
                <arg2p-editor
                    id="query"
                    kind="query"
                    label="Query"
                    placeholder=${QUERY}
                    .engine=${this.engine}
                    .value=${this.query}
                    @arg2p-change=${own(({ value }: { value: string }) => this.edit('query', value))}
                    @arg2p-submit=${own(() => this.run())}
                ></arg2p-editor>
                ${this.busy
                    ? html`<button type="button" class="stop" @click=${() => this.stop()}><span class="spinner" aria-hidden="true"></span>Stop</button>`
                    : html`<button type="button" class="run" title="Solve the query (Enter)" ?disabled=${this.engine === null} @click=${() => this.run()}>Run</button>`}
            </div>
            <div class="tabs" part="tabs" role="tablist" aria-label="Solver" @keydown=${(e: KeyboardEvent) => this.moveTab(e)}>
                ${tab('results', 'Solutions')} ${this.theoryTab ? tab('theory', 'Theory') : nothing} ${tab('flags', 'Flags')}
                ${tab('console', this.output === '' ? 'Console' : 'Console •')} ${this.hosted.map(it => tab(it.id, it.label))}
                <span class="profile" title=${info?.help ?? ''}>${info?.label ?? this.profile}</span>
                <span class="spacer"></span>
                <slot name="actions"></slot>
            </div>
            ${this.notice === null
                ? nothing
                : html`<div class="notice" part="notice" role="status">
                      <span>${this.notice}</span><button type="button" class="dismiss" title="Dismiss" aria-label="Dismiss" @click=${() => (this.notice = null)}>×</button>
                  </div>`}
            <div
                class="panel results ${this.solutionsCollapsed ? 'collapsed' : ''}"
                part="results"
                role="tabpanel"
                id="panel-results"
                aria-labelledby="tab-results"
                ?hidden=${shown !== 'results'}
            >
                <arg2p-solutions
                    id="solutions"
                    ?hide-exhausted-no=${abstract}
                    @arg2p-solution=${(e: CustomEvent<Solved>) => this.show(e.detail)}
                    @arg2p-busy=${own(({ busy }: { busy: boolean }) => (this.computing = busy))}
                    @arg2p-failure=${own(({ error }: { error: EngineError }) => this.failed(error))}
                    @arg2p-error-select=${own(({ error }: { error: EngineError }) => this.reveal(error))}
                ></arg2p-solutions>
                <div class="split">
                    <button
                        type="button"
                        class="collapse"
                        part="collapse"
                        title=${this.solutionsCollapsed ? 'Show the solutions' : 'Hide the solutions, for a wider graph'}
                        aria-label=${this.solutionsCollapsed ? 'Show the solutions' : 'Hide the solutions'}
                        aria-expanded=${!this.solutionsCollapsed}
                        aria-controls="solutions"
                        @click=${() => this.toggleSolutions()}
                    >
                        <span aria-hidden="true">${this.solutionsCollapsed ? '›' : '‹'}</span>${this.solutionsCollapsed ? html`<span class="strip">Solutions</span>` : nothing}
                    </button>
                    <slot name="split"></slot>
                </div>
                <div class="view" part="view">
                    <arg2p-graph
                        ?hidden=${abstract}
                        file-name=${this.fileName}
                        .solution=${this.solved}
                        .linked=${this.linked}
                        .passages=${this.passages}
                        .positions=${this.positions}
                        @arg2p-positions=${(e: CustomEvent<{ positions: Positions }>) => (this.positions = e.detail.positions)}
                    ></arg2p-graph>
                    <arg2p-framework
                        ?hidden=${!abstract}
                        file-name=${this.fileName}
                        .theory=${this.theory}
                        .graph=${this.solved?.solution.graph ?? null}
                        .linked=${this.linked}
                        .passages=${this.passages}
                        .positions=${this.positions}
                        @arg2p-positions=${(e: CustomEvent<{ positions: Positions }>) => (this.positions = e.detail.positions)}
                        @arg2p-theory-change=${own(({ theory }: { theory: string }) => this.edit('theory', theory))}
                    ></arg2p-framework>
                </div>
            </div>
            ${this.theoryTab
                ? html`<div class="panel theory" role="tabpanel" id="panel-theory" aria-labelledby="tab-theory" ?hidden=${shown !== 'theory'}>
                      <arg2p-editor
                          id="theory"
                          kind="theory"
                          label="Theory"
                          placeholder="A theory: rules, facts, and its flags (graphBuildMode(abstract). for an abstract framework)"
                          .engine=${this.engine}
                          .value=${this.theory}
                          @arg2p-change=${own(({ value }: { value: string }) => this.edit('theory', value))}
                      ></arg2p-editor>
                  </div>`
                : nothing}
            <div class="panel flags" role="tabpanel" id="panel-flags" aria-labelledby="tab-flags" ?hidden=${shown !== 'flags'}>
                <arg2p-flags
                    .engine=${this.engine}
                    .flags=${this.flags}
                    .theory=${this.theory}
                    @arg2p-flags-change=${own(({ flags }: { flags: string }) => this.edit('flags', flags))}
                ></arg2p-flags>
                <details class="flags-text">
                    <summary>Flags as text</summary>
                    <p class="hint">The facts the solver is given: edit them freely, the settings above follow.</p>
                    <arg2p-editor
                        id="flags"
                        kind="flags"
                        label="Flags"
                        .engine=${this.engine}
                        .value=${this.flags}
                        @arg2p-change=${own(({ value }: { value: string }) => this.edit('flags', value))}
                    ></arg2p-editor>
                </details>
            </div>
            <div class="panel console" role="tabpanel" id="panel-console" aria-labelledby="tab-console" ?hidden=${shown !== 'console'}>
                ${this.output === '' ? html`<p class="hint">What the theory and the query print (write/1, say) shows here.</p>` : html`<pre>${this.output}</pre>`}
            </div>
            <div class="panel hosted" id="panel-hosted" ?hidden=${!this.hosted.some(it => it.id === shown)}><slot name="tab"></slot></div>
        `;
    }

    static override styles = [
        tokens,
        buttons,
        css`
            /* the page gives it its height: that of a webview, of an iframe, of a column */
            :host {
                display: flex;
                flex-direction: column;
                gap: 8px;
                min-height: 0;
                box-sizing: border-box;
                container: arg2p-solver / inline-size;
                font-size: 13px;
            }
            [hidden] {
                display: none !important;
            }
            .query {
                display: flex;
                align-items: center;
                gap: 8px;
            }
            .prompt {
                font-family: var(--_mono);
                color: var(--_muted);
            }
            #query {
                flex: 1;
                min-width: 0;
            }
            .run,
            .stop {
                gap: 6px;
                font-weight: 600;
                padding: 5px 16px;
            }
            .run {
                color: var(--_surface);
                background: var(--_accent);
                border-color: var(--_accent);
            }
            .run:hover:not(:disabled) {
                background: color-mix(in srgb, var(--_accent) 85%, black);
            }
            .stop {
                color: var(--_error);
                border-color: var(--_error);
            }
            .spinner {
                width: 10px;
                height: 10px;
                border: 2px solid currentColor;
                border-right-color: transparent;
                border-radius: 50%;
                animation: spin 0.8s linear infinite;
            }
            @keyframes spin {
                to {
                    transform: rotate(360deg);
                }
            }
            @media (prefers-reduced-motion: reduce) {
                .spinner {
                    animation: none;
                }
            }
            .tabs {
                display: flex;
                flex-wrap: wrap;
                align-items: center;
                gap: 4px;
                border-bottom: 1px solid var(--_border);
            }
            .tabs > button {
                border: none;
                border-bottom: 2px solid transparent;
                border-radius: 0;
                background: none;
                color: var(--_muted);
                padding: 4px 10px;
            }
            .tabs > button.active {
                color: var(--_text);
                border-bottom-color: var(--_accent);
            }
            .profile {
                font-size: 11px;
                padding: 0 6px;
                border-radius: 8px;
                color: var(--_accent);
                border: 1px solid var(--_accent);
            }
            .spacer {
                flex: 1;
            }
            .notice {
                display: flex;
                align-items: center;
                gap: 8px;
                padding: 4px 4px 4px 10px;
                border-radius: 6px;
                background: color-mix(in srgb, var(--_error) 10%, transparent);
            }
            .notice span {
                flex: 1;
            }
            .dismiss {
                border: none;
                background: none;
                color: var(--_muted);
            }
            .panel {
                flex: 1;
                min-height: 0;
            }
            .results {
                display: grid;
                grid-template-columns: minmax(180px, var(--arg2p-solutions-width, 30%)) auto minmax(240px, 1fr);
                grid-template-rows: minmax(0, 1fr);
            }
            arg2p-solutions {
                overflow: auto;
                min-height: 0;
            }
            .split {
                position: relative;
                display: flex;
                min-width: 8px;
            }
            /* the handle that folds the solutions away, in the middle of the split, and the strip they fold into */
            .collapse {
                position: absolute;
                z-index: 2;
                top: 50%;
                left: 50%;
                transform: translate(-50%, -50%);
                display: flex;
                align-items: center;
                justify-content: center;
                width: 18px;
                height: 28px;
                padding: 0;
                font: inherit;
                font-size: 15px;
                line-height: 1;
                color: var(--_muted);
                background: var(--_surface);
                border: 1px solid var(--_border);
                border-radius: 9px;
                cursor: pointer;
            }
            .collapse:hover {
                color: var(--_accent);
                border-color: var(--_accent);
            }
            .results.collapsed {
                grid-template-columns: auto minmax(240px, 1fr);
            }
            .results.collapsed arg2p-solutions,
            .results.collapsed slot[name='split']::slotted(*) {
                display: none;
            }
            .results.collapsed .collapse {
                position: static;
                transform: none;
                flex-direction: column;
                justify-content: flex-start;
                gap: 8px;
                width: 28px;
                height: auto;
                margin-right: 8px;
                padding: 8px 0;
                border-radius: 6px;
            }
            .strip {
                writing-mode: vertical-rl;
                font-size: 13px;
            }
            .view {
                display: flex;
                min-height: 0;
                min-width: 0;
            }
            .view > * {
                flex: 1;
                min-width: 0;
                border: 1px solid var(--_border);
                border-radius: 6px;
                overflow: hidden;
            }
            /* nothing to draw and no stages (a plain Prolog query): the solutions take the room */
            .results:has(arg2p-graph[empty]:not([hidden])) {
                grid-template-columns: 1fr;
            }
            .results:has(arg2p-graph[empty]:not([hidden])) .split,
            .results:has(arg2p-graph[empty]:not([hidden])) .view {
                display: none;
            }
            .results:has(arg2p-graph[empty]:not([hidden])) arg2p-solutions {
                display: block;
            }
            .theory {
                display: flex;
            }
            #theory {
                flex: 1;
            }
            .flags,
            .console,
            .hosted {
                overflow: auto;
            }
            ::slotted([hidden]) {
                display: none !important;
            }
            .flags-text summary {
                cursor: pointer;
                padding: 8px 0;
                color: var(--_muted);
            }
            .hint {
                color: var(--_muted);
            }
            #flags {
                height: 200px;
            }
            pre {
                margin: 0;
                padding: 8px;
                font-family: var(--_mono);
                white-space: pre-wrap;
            }
            /* narrow: the graph under the solutions, the panel scrolled if the page gives less room */
            @container arg2p-solver (max-width: 600px) {
                .panel {
                    overflow: auto;
                }
                .results,
                .results.collapsed {
                    grid-template-columns: 1fr;
                    grid-template-rows: none;
                    align-content: start;
                    gap: 8px;
                }
                .results.collapsed arg2p-solutions {
                    display: block;
                }
                .split {
                    display: none;
                }
                .view {
                    height: max(320px, 60vh);
                }
            }
        `,
    ];
}

define('arg2p-solver', Arg2pSolver);

declare global {
    interface HTMLElementTagNameMap {
        'arg2p-solver': Arg2pSolver;
    }
}
