import '@tuprolog/arg2p-ui';
import type { Arg2pEditor, Arg2pFramework, Arg2pGraph, Arg2pSolutions } from '@tuprolog/arg2p-ui';
import { DEFAULT_PROFILE, profileInfo, statedProfile, type EngineError, type Positions, type Run, type SolutionData } from '@tuprolog/arg2p-ui/data';
import { JsEngine, type Answer, type Channel } from '@tuprolog/arg2p-ui/js';
import { LitElement, css, html, nothing } from 'lit';

// The solver of a theory, in the webview of VS Code: the components of @tuprolog/arg2p-ui, on an engine that is the
// language server (the extension passes the messages of the engine on, see bridge.ts). The theory is the document the
// panel belongs to: the extension sends it as it changes, and the drawing of a framework edits it.

interface VsCode {
    postMessage(message: unknown): void;
}

declare function acquireVsCodeApi(): VsCode;

const vscode = acquireVsCodeApi();

type Panel = 'results' | 'flags' | 'console';

// The channel of the engine: to the extension, and back from it
let channel: Channel | null = null;
const engine = new JsEngine(() => {
    const opened: Channel = {
        onmessage: null,
        postMessage: request => vscode.postMessage({ type: 'engine', request }),
        // a resolution is stopped by restarting the server
        terminate: () => vscode.postMessage({ type: 'restart' }),
    };
    channel = opened;
    return opened;
});

export class Arg2pSolver extends LitElement {
    static override properties = {
        name: { state: true },
        theory: { state: true },
        query: { state: true },
        flags: { state: true },
        panel: { state: true },
        drawing: { state: true },
        busy: { state: true },
        output: { state: true },
        notice: { state: true },
        positions: { state: true },
    };

    private name = '';
    private theory = '';
    private query = '';
    private flags = '';
    private panel: Panel = 'results';
    private drawing = true;
    private busy = false;
    private output = '';
    private notice: string | null = null;
    private positions: Positions | null = null;
    private runs = 0;

    override connectedCallback(): void {
        super.connectedCallback();
        window.addEventListener('message', event => this.receive(event.data as ToPage));
        vscode.postMessage({ type: 'ready' });
    }

    private receive(message: ToPage): void {
        if (message.type === 'engine') channel?.onmessage?.({ data: message.answer });
        else if (message.type === 'theory') this.theory = message.theory;
        else if (message.type === 'document') {
            this.name = message.name;
            this.theory = message.theory;
            this.query = message.query;
            this.flags = message.flags;
        }
    }

    private get profile(): string {
        return statedProfile(this.theory) ?? statedProfile(this.flags) ?? DEFAULT_PROFILE;
    }

    private get abstract(): boolean {
        return this.profile === 'abstract';
    }

    private find<T extends Element>(selector: string): T | null {
        return this.renderRoot.querySelector<T>(selector);
    }

    private remember(): void {
        vscode.postMessage({ type: 'state', query: this.query, flags: this.flags });
    }

    private async run(): Promise<void> {
        const run = ++this.runs;
        const solutions = this.find<Arg2pSolutions>('arg2p-solutions')!;
        this.output = '';
        this.notice = null;
        this.panel = 'results';
        this.find<Arg2pGraph>('arg2p-graph')?.clear();
        this.busy = true;
        try {
            const started = await engine.solve({ theory: this.theory, query: this.query, flags: this.flags }, text => {
                if (run === this.runs) this.output += text;
            });
            if (run === this.runs) await solutions.start(started);
        } catch (error) {
            if (run === this.runs) solutions.fail(error as EngineError);
        } finally {
            if (run === this.runs) this.busy = solutions.busy;
        }
    }

    private show(solution: SolutionData, run: Run | null): void {
        this.find<Arg2pGraph>('arg2p-graph')?.showSolution(solution, run);
        const drawing = this.find<Arg2pFramework>('arg2p-framework');
        if (drawing !== null) drawing.graph = solution.graph;
    }

    private reveal(error: EngineError): void {
        if (error.syntax?.where === 'query') this.find<Arg2pEditor>('#query')?.report(error.syntax);
        else if (error.syntax?.where === 'flags') {
            this.panel = 'flags';
            this.updateComplete.then(() => this.find<Arg2pEditor>('#flags')?.report(error.syntax!));
        } else if (error.syntax) this.notice = `The theory does not parse at line ${error.syntax.line}: see the problems of the document.`;
    }

    protected override render() {
        const tab = (panel: Panel, text: string) =>
            html`<button type="button" role="tab" class=${this.panel === panel ? 'active' : ''} aria-selected=${this.panel === panel} @click=${() => (this.panel = panel)}>${text}</button>`;
        const info = profileInfo(this.profile);
        return html`
            <div class="query">
                <span class="prompt">?-</span>
                <arg2p-editor
                    id="query"
                    kind="query"
                    label="Query"
                    placeholder="buildLabelSets(In, Out, Und)"
                    .engine=${engine}
                    .value=${this.query}
                    @arg2p-change=${(e: CustomEvent<{ value: string }>) => {
                        this.query = e.detail.value;
                        this.remember();
                    }}
                    @arg2p-submit=${() => this.run()}
                ></arg2p-editor>
                ${this.busy
                    ? html`<button type="button" class="stop" @click=${() => engine.stop()}>Stop</button>`
                    : html`<button type="button" class="run" title="Solve the query (Enter)" @click=${() => this.run()}>Run</button>`}
            </div>
            <div class="tabs" role="tablist">
                ${tab('results', 'Solutions')} ${tab('flags', 'Flags')} ${tab('console', this.output === '' ? 'Console' : 'Console •')}
                <span class="profile" title=${info?.help ?? ''}>${info?.label ?? this.profile}</span>
                <span class="spacer"></span>
                ${this.abstract && this.panel === 'results'
                    ? html`<span class="switch">
                          <button type="button" class=${this.drawing ? 'active' : ''} @click=${() => (this.drawing = true)}>Drawing</button>
                          <button type="button" class=${this.drawing ? '' : 'active'} @click=${() => (this.drawing = false)}>Graph</button>
                      </span>`
                    : nothing}
            </div>
            ${this.notice === null ? nothing : html`<div class="notice">${this.notice}</div>`}
            <div class="panel results" ?hidden=${this.panel !== 'results'}>
                <arg2p-solutions
                    ?hide-exhausted-no=${this.abstract}
                    @arg2p-solution=${(e: CustomEvent<{ solution: SolutionData; run: Run | null }>) => this.show(e.detail.solution, e.detail.run)}
                    @arg2p-busy=${(e: CustomEvent<{ busy: boolean }>) => (this.busy = e.detail.busy)}
                    @arg2p-failure=${(e: CustomEvent<{ error: EngineError }>) => (this.notice = e.detail.error.stopped ? 'Stopped.' : e.detail.error.message)}
                    @arg2p-error-select=${(e: CustomEvent<{ error: EngineError }>) => this.reveal(e.detail.error)}
                ></arg2p-solutions>
                <div class="view">
                    <arg2p-graph
                        ?hidden=${this.abstract && this.drawing}
                        file-name=${this.name}
                        .positions=${this.positions}
                        @arg2p-positions=${(e: CustomEvent<{ positions: Positions }>) => (this.positions = e.detail.positions)}
                    ></arg2p-graph>
                    <arg2p-framework
                        ?hidden=${!(this.abstract && this.drawing)}
                        file-name=${this.name}
                        .theory=${this.theory}
                        .positions=${this.positions}
                        @arg2p-positions=${(e: CustomEvent<{ positions: Positions }>) => (this.positions = e.detail.positions)}
                        @arg2p-theory-change=${(e: CustomEvent<{ theory: string }>) => {
                            this.theory = e.detail.theory;
                            vscode.postMessage({ type: 'edit', theory: e.detail.theory });
                        }}
                    ></arg2p-framework>
                </div>
            </div>
            <div class="panel flags" ?hidden=${this.panel !== 'flags'}>
                <arg2p-flags
                    .engine=${engine}
                    .flags=${this.flags}
                    .theory=${this.theory}
                    @arg2p-flags-change=${(e: CustomEvent<{ flags: string }>) => {
                        this.flags = e.detail.flags;
                        this.remember();
                    }}
                ></arg2p-flags>
                <details>
                    <summary>Flags as text</summary>
                    <arg2p-editor
                        id="flags"
                        kind="flags"
                        label="Flags"
                        .engine=${engine}
                        .value=${this.flags}
                        @arg2p-change=${(e: CustomEvent<{ value: string }>) => {
                            this.flags = e.detail.value;
                            this.remember();
                        }}
                    ></arg2p-editor>
                </details>
            </div>
            <div class="panel console" ?hidden=${this.panel !== 'console'}>
                ${this.output === '' ? html`<p class="hint">What the theory and the query print shows here.</p>` : html`<pre>${this.output}</pre>`}
            </div>
        `;
    }

    static override styles = css`
        :host {
            display: flex;
            flex-direction: column;
            gap: 8px;
            height: 100vh;
            padding: 8px 12px;
            box-sizing: border-box;
            color: var(--arg2p-text);
            font-family: var(--arg2p-font);
            font-size: var(--vscode-font-size, 13px);
        }
        [hidden] {
            display: none !important;
        }
        button {
            font: inherit;
            color: var(--vscode-button-secondaryForeground, var(--arg2p-text));
            background: var(--vscode-button-secondaryBackground, transparent);
            border: 1px solid var(--arg2p-border);
            border-radius: 2px;
            padding: 3px 10px;
            cursor: pointer;
        }
        .query {
            display: flex;
            align-items: center;
            gap: 8px;
        }
        .prompt {
            font-family: var(--arg2p-mono);
            color: var(--arg2p-muted);
        }
        #query {
            flex: 1;
        }
        .run {
            color: var(--vscode-button-foreground);
            background: var(--vscode-button-background);
            border-color: var(--vscode-button-background);
        }
        .run:hover {
            background: var(--vscode-button-hoverBackground);
        }
        .stop {
            color: var(--arg2p-error);
            border-color: var(--arg2p-error);
        }
        .tabs {
            display: flex;
            align-items: center;
            gap: 4px;
            border-bottom: 1px solid var(--arg2p-border);
        }
        .tabs > button {
            border: none;
            border-bottom: 2px solid transparent;
            border-radius: 0;
            background: none;
            color: var(--arg2p-muted);
        }
        .tabs > button.active {
            color: var(--arg2p-text);
            border-bottom-color: var(--arg2p-accent);
        }
        .profile {
            font-size: 11px;
            padding: 0 6px;
            border-radius: 8px;
            border: 1px solid var(--arg2p-accent);
            color: var(--arg2p-accent);
        }
        .spacer {
            flex: 1;
        }
        .switch button.active {
            color: var(--vscode-button-foreground);
            background: var(--vscode-button-background);
        }
        .notice {
            padding: 4px 8px;
            color: var(--vscode-inputValidation-warningForeground, var(--arg2p-text));
            background: var(--vscode-inputValidation-warningBackground, transparent);
            border: 1px solid var(--vscode-inputValidation-warningBorder, var(--arg2p-border));
        }
        .panel {
            flex: 1;
            min-height: 0;
        }
        .results {
            display: grid;
            grid-template-columns: minmax(180px, 1fr) 2.5fr;
            gap: 8px;
        }
        arg2p-solutions {
            overflow: auto;
        }
        .view {
            display: flex;
            min-height: 0;
        }
        .view > * {
            flex: 1;
            min-width: 0;
            border: 1px solid var(--arg2p-border);
        }
        .results:has(arg2p-graph[empty]:not([hidden])) {
            grid-template-columns: 1fr;
        }
        .results:has(arg2p-graph[empty]:not([hidden])) .view {
            display: none;
        }
        .flags,
        .console {
            overflow: auto;
        }
        #flags {
            height: 160px;
        }
        pre {
            margin: 0;
            font-family: var(--arg2p-mono);
            white-space: pre-wrap;
        }
        .hint {
            color: var(--arg2p-muted);
        }
        @media (max-width: 700px) {
            .results {
                grid-template-columns: 1fr;
            }
            .view {
                height: 60vh;
            }
        }
    `;
}

type ToPage =
    | { type: 'document'; name: string; theory: string; query: string; flags: string }
    | { type: 'theory'; theory: string }
    | { type: 'engine'; answer: Answer };

// The colours of the components, those of the theme of VS Code
const theme = document.createElement('style');
theme.textContent = `
    :root {
        --arg2p-surface: var(--vscode-editor-background);
        --arg2p-surface-2: var(--vscode-sideBar-background, var(--vscode-editor-background));
        --arg2p-hover: var(--vscode-list-hoverBackground);
        --arg2p-border: var(--vscode-panel-border, var(--vscode-widget-border, #8884));
        --arg2p-text: var(--vscode-foreground);
        --arg2p-muted: var(--vscode-descriptionForeground);
        --arg2p-accent: var(--vscode-textLink-foreground);
        --arg2p-error: var(--vscode-errorForeground);
        --arg2p-font: var(--vscode-font-family);
        --arg2p-mono: var(--vscode-editor-font-family);
        --arg2p-atom: var(--vscode-debugTokenExpression-string, #ce9178);
        --arg2p-functor: var(--vscode-symbolIcon-functionForeground, #dcdcaa);
        --arg2p-comment: var(--vscode-editorLineNumber-foreground);
    }
    body {
        margin: 0;
        padding: 0;
        background: var(--vscode-editor-background);
    }
`;
document.head.appendChild(theme);

customElements.define('arg2p-solver', Arg2pSolver);
