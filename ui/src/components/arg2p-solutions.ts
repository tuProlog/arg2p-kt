import { LitElement, css, html, nothing, type PropertyValues } from 'lit';
import type { EngineError, Run, SolutionData, Solved } from '../engine.js';
import { emit } from '../events.js';
import { listItems } from '../formats/terms.js';
import { tokens } from '../theme.js';
import { buttons, chip, chips, define } from './shared.js';

// Bindings named after a label (In, Out, Und) hold sets of arguments or statements: shown as chips coloured like the
// graph
const LABELS = ['in', 'out', 'und'];

interface Entry {
    readonly solution: SolutionData;
    // its number among the yes solutions, 0 for the others
    readonly accepted: number;
}

/** What the solutions show, to show again (see save and restore): the run, its solutions, the one shown, an error */
export interface SolutionsState {
    readonly run: Run | null;
    readonly entries: readonly Entry[];
    readonly error: EngineError | null;
    /** The solution shown, by its place among the entries; -1 for none */
    readonly shown: number;
}

/**
 * The solutions of a query, asked for one at a time (Next): yes with its bindings, no, or halt with its error; an
 * error that kept the query from running shows alone. Picking a yes solution shows it again (its graph, say).
 *
 * Events: `arg2p-solution` with the solution and its run (Solved), when one is shown (the newest, or one picked);
 * `arg2p-busy` with whether a solution is being computed; `arg2p-failure` with the EngineError of a computation that
 * failed (stopped, say); `arg2p-error-select` with the error shown, when it is picked (to go where it is).
 *
 * Attributes: `hide-exhausted-no` hides the no that only tells that the yes solutions are over; `trivial` is set
 * when there is nothing to show besides the query itself (a page may hide the solutions then), `busy` while a
 * solution is being computed.
 */
export class Arg2pSolutions extends LitElement {
    static override properties = {
        hideExhaustedNo: { type: Boolean, attribute: 'hide-exhausted-no' },
        working: { state: true },
        run: { state: true },
        entries: { state: true },
        error: { state: true },
        shown: { state: true },
    };

    hideExhaustedNo = false;

    // whether a solution is being computed
    private working = false;
    private run: Run | null = null;
    private entries: Entry[] = [];
    private error: EngineError | null = null;
    private shown: Entry | null = null;

    /** Shows the solutions of [run], from its first one */
    start(run: Run): Promise<void> {
        this.reset();
        this.run = run;
        return this.next();
    }

    /** Shows [error], which kept a query from running */
    fail(error: EngineError): void {
        this.reset();
        this.error = error;
    }

    clear(): void {
        this.reset();
    }

    /** What is shown now, to show it again with restore */
    save(): SolutionsState {
        return { run: this.run, entries: this.entries, error: this.error, shown: this.shown === null ? -1 : this.entries.indexOf(this.shown) };
    }

    /**
     * Shows [state] again, as saved: its run goes on from where it was (a solution being computed meanwhile is let go,
     * and no solution is told as shown: the page has it)
     */
    restore(state: SolutionsState): void {
        this.reset();
        this.run = state.run;
        this.entries = [...state.entries];
        this.error = state.error;
        this.shown = this.entries[state.shown] ?? null;
    }

    /** Whether a solution is being computed */
    get busy(): boolean {
        return this.working;
    }

    /**
     * Whether there is nothing to show besides the query: a plain yes with no bindings only repeats it. The solutions
     * are worth showing for bindings, failures and errors, and as soon as there is a second solution to pick from.
     */
    get trivial(): boolean {
        return (
            this.error === null &&
            this.entries.every(it => it.solution.result === 'yes' && it.solution.bindings.length === 0) &&
            this.entries.filter(it => it.solution.result === 'yes').length <= 1
        );
    }

    /** Whether there is another solution to ask for */
    get hasNext(): boolean {
        return this.run !== null && !this.busy && this.run.hasNext();
    }

    /** Asks for the next solution: the page stays responsive meanwhile. A run replaced meanwhile is let go */
    next(): Promise<void> {
        const run = this.run;
        if (run === null || this.busy || !run.hasNext()) return Promise.resolve();
        this.setBusy(true);
        return run.next().then(
            solution => {
                if (run !== this.run) return;
                this.setBusy(false);
                this.add(solution);
            },
            (error: EngineError) => {
                if (run !== this.run) return;
                this.setBusy(false);
                emit(this, 'arg2p-failure', { error });
            },
        );
    }

    private reset(): void {
        this.run = null;
        this.entries = [];
        this.error = null;
        this.shown = null;
        this.setBusy(false);
    }

    private setBusy(busy: boolean): void {
        if (this.working === busy) return;
        this.working = busy;
        emit(this, 'arg2p-busy', { busy });
    }

    private add(solution: SolutionData): void {
        const accepted = this.entries.filter(it => it.accepted > 0).length;
        // Once a solution has been accepted, a trailing no only marks the end of the enumeration. A leading one still
        // means something (no extension under the semantics chosen), so it is never hidden.
        if (this.hideExhaustedNo && solution.result === 'no' && accepted > 0) return;
        const entry = { solution, accepted: solution.result === 'yes' ? accepted + 1 : 0 };
        this.entries = [...this.entries, entry];
        // the newest solution is the one shown
        if (solution.result === 'yes') this.show(entry);
    }

    private show(entry: Entry): void {
        this.shown = entry;
        const solved: Solved = { solution: entry.solution, run: this.run };
        emit(this, 'arg2p-solution', solved);
    }

    protected override willUpdate(): void {
        this.toggleAttribute('trivial', this.trivial);
        this.toggleAttribute('busy', this.busy);
    }

    protected override updated(changed: PropertyValues): void {
        if (changed.has('entries')) this.renderRoot.querySelector('.solution:last-child')?.scrollIntoView?.({ block: 'nearest' });
    }

    // What readers of the screen are told as it happens lives in a region of its own, there all along
    protected override render() {
        return html`${this.renderSolutions()}<div class="live" role="status">${this.busy ? 'Solving…' : ''}</div>`;
    }

    private renderSolutions() {
        if (this.error !== null) return this.renderError(this.error);
        if (this.run === null) return nothing;
        return html`
            <div class="head">
                <div class="query">?- ${this.run.query}</div>
                <button type="button" class="next" title="The next solution" ?disabled=${!this.hasNext} @click=${() => this.next()}>Next</button>
            </div>
            <ol class="list">
                ${this.entries.map(entry => this.renderEntry(entry))}
                ${this.busy ? html`<li class="pending" aria-hidden="true">Solving…</li>` : nothing}
            </ol>
        `;
    }

    // An error that tells where it is takes there: its header a button, the whole of it a target for the pointer
    private renderError(error: EngineError) {
        const select = error.syntax === null ? null : () => emit(this, 'arg2p-error-select', { error });
        return html`
            <ol class="list">
                <li class="solution halt ${select !== null ? 'selectable' : ''}" @click=${select}>
                    ${select !== null ? html`<button type="button" class="pick" title="Go to the error">${header('error', error.name)}</button>` : header('error', error.name)}
                    <div class="error">${error.message}</div>
                </li>
            </ol>
        `;
    }

    private renderEntry(entry: Entry) {
        const { solution, accepted } = entry;
        if (solution.result === 'no') {
            const any = this.entries.some(it => it.accepted > 0);
            return html`<li class="solution no">${header('no', any ? 'No more solutions' : 'No solutions')}</li>`;
        }
        if (solution.result === 'halt') {
            return html`<li class="solution halt">${header('halt', 'Halted')}<div class="error">${solution.error ?? ''}</div></li>`;
        }
        // its header a button, the whole of it a target for the pointer
        return html`
            <li class="solution yes selectable ${this.shown === entry ? 'shown' : ''}" @click=${() => this.show(entry)}>
                <button type="button" class="pick" aria-pressed=${this.shown === entry} title="Show the graph of this solution">${header('yes', `Solution ${accepted}`)}</button>
                ${solution.bindings.length > 0 ? bindings(solution.bindings) : html`<div class="term">${solution.query}</div>`}
            </li>
        `;
    }

    static override styles = [
        tokens,
        buttons,
        chips,
        css`
            :host {
                display: block;
                font-size: 14px;
            }
            .head {
                display: flex;
                align-items: baseline;
                gap: 8px;
                padding: 0 2px 8px 2px;
            }
            .query {
                flex: 1;
                font-family: var(--_mono);
                font-size: 13px;
                color: var(--_muted);
                word-break: break-word;
            }
            button {
                font-size: 13px;
                padding: 2px 10px;
            }
            .list {
                list-style: none;
                margin: 0;
                padding: 0;
            }
            .solution {
                border: 1px solid var(--_border);
                border-left: 4px solid var(--_accent);
                border-radius: 6px;
                padding: 8px 10px;
                margin-bottom: 8px;
                background: var(--_surface);
            }
            .selectable {
                cursor: pointer;
            }
            .selectable:hover {
                background: var(--_hover);
            }
            .list .pick,
            .list .pick:hover:not(:disabled) {
                display: block;
                width: 100%;
                padding: 0;
                border: none;
                background: none;
                color: inherit;
                font: inherit;
                text-align: start;
            }
            .pick:focus {
                outline: none;
            }
            .selectable:has(.pick:focus-visible),
            .shown {
                box-shadow: 0 0 0 2px color-mix(in srgb, var(--_accent) 45%, transparent);
            }
            .solution.no {
                border-left-color: var(--_no);
            }
            .solution.halt {
                border-left-color: var(--_error);
            }
            .live {
                position: absolute;
                width: 1px;
                height: 1px;
                overflow: hidden;
                clip-path: inset(50%);
                white-space: nowrap;
            }
            .header {
                display: flex;
                align-items: center;
                gap: 8px;
            }
            .badge {
                font-size: 11px;
                font-weight: bold;
                text-transform: uppercase;
                letter-spacing: 0.04em;
                color: var(--_surface);
                background: var(--_accent);
                border-radius: 4px;
                padding: 1px 6px;
            }
            /* the yellow of no is the same in every theme: dark is written on it */
            .no .badge {
                color: #212529;
                background: var(--_no);
            }
            .halt .badge {
                background: var(--_error);
            }
            .term,
            .error,
            .bindings {
                margin: 8px 0 0 0;
                font-family: var(--_mono);
                font-size: 13px;
                word-break: break-word;
            }
            .error {
                color: var(--_error);
            }
            .bindings {
                display: grid;
                grid-template-columns: max-content 1fr;
                column-gap: 10px;
                row-gap: 6px;
                align-items: baseline;
            }
            dt {
                font-weight: bold;
            }
            dt::after {
                content: ' =';
                color: var(--_muted);
                font-weight: normal;
            }
            dd {
                margin: 0;
                display: flex;
                flex-wrap: wrap;
                gap: 4px;
            }
            .empty,
            .pending {
                color: var(--_muted);
            }
            .pending {
                font-style: italic;
                padding: 4px 2px;
            }
        `,
    ];
}

// Phrasing alone: it may be the content of a button
function header(badge: string, title: string) {
    return html`<span class="header"><span class="badge">${badge}</span><span>${title}</span></span>`;
}

// `Variable = value` rows. A value of a label variable that is a list shows its items as chips; any other value keeps
// its Prolog form, where a list and its single element would otherwise look the same.
function bindings(list: SolutionData['bindings']) {
    return html`
        <dl class="bindings">
            ${list.map(({ name, value }) => {
                const label = name.toLowerCase();
                const items = LABELS.includes(label) ? listItems(value) : null;
                return html`
                    <dt>${name}</dt>
                    <dd>
                        ${items === null ? value
                        : items.length === 0 ? html`<span class="empty">[ ]</span>`
                        : items.map(item => chip(single(item), label))}
                    </dd>
                `;
            })}
        </dl>
    `;
}

// A statement of a set of labelled statements is a list of one term, `[flies(tweety)]`: shown as the term
function single(item: string): string {
    const items = listItems(item);
    return items !== null && items.length === 1 ? items[0] : item;
}

define('arg2p-solutions', Arg2pSolutions);

declare global {
    interface HTMLElementTagNameMap {
        'arg2p-solutions': Arg2pSolutions;
    }
}
