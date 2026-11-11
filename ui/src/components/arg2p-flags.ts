import { LitElement, css, html, nothing, type PropertyValues } from 'lit';
import type { Engine } from '../engine.js';
import { emit } from '../events.js';
import { DEFAULTS, FLAG_GROUPS, FLAGS, PROFILES, profileInfo, type Flag, type FlagGroup, type Settings } from '../flags/schema.js';
import { expanded, profileOf, statedFlags, statedProfile, withNone, withSwitch, withValues, without, type Stated } from '../flags/text.js';
import { tokens } from '../theme.js';
import { define } from './shared.js';

// How long the flags and the theory are let be before the engine is asked what is in effect
const SETTLE = 250;

/** Where the value of a flag in effect comes from */
export type Source = 'theory' | 'flags' | 'profile' | 'default';

/**
 * The flags of the solver, as controls over their text (Prolog facts, the source of truth: every change rewrites the
 * lines of its flag alone). The profile comes first: it presets the flags its framework needs, and the flags that mean
 * something for it are shown, the others folded away. Every flag shows the value in effect for the theory, as the
 * engine tells it, and where it comes from: the theory (which wins: it is changed in the theory), the flags, the
 * profile, or the default. A flag the flags state can be reset, to take the value of the profile again.
 *
 * Events: `arg2p-flags-change` with the flags, after every change made with the controls.
 */
export class Arg2pFlags extends LitElement {
    static override properties = {
        flags: {},
        theory: {},
        engine: { attribute: false },
        settings: { state: true },
    };

    /** The flags, as Prolog facts */
    flags = '';
    /** The theory the flags are for: it may state flags, and its profile */
    theory = '';
    /** What tells the settings in effect; without it, the panel shows what the flags state */
    engine: Pick<Engine, 'settings'> | null = null;

    private settings: Settings | null = null;
    // what the theory and the flags state, read once they change
    private stated: { theory: Stated; flags: Stated } = { theory: new Map(), flags: new Map() };
    private timer: ReturnType<typeof setTimeout> | undefined;
    private asked = 0;

    protected override willUpdate(changed: PropertyValues): void {
        if (changed.has('theory') || changed.has('flags')) this.stated = { theory: statedFlags(this.theory), flags: statedFlags(this.flags) };
    }

    protected override updated(changed: PropertyValues): void {
        if (changed.has('flags') || changed.has('theory') || changed.has('engine')) this.ask();
        // the value of a select set once its options are there (as rendered, some DOMs pick another)
        this.renderRoot.querySelectorAll<HTMLSelectElement>('select[data-value]').forEach(it => {
            it.value = it.dataset.value!;
        });
    }

    override disconnectedCallback(): void {
        super.disconnectedCallback();
        clearTimeout(this.timer);
    }

    // The settings in effect, asked once the typing settles: a later answer wins
    private ask(): void {
        clearTimeout(this.timer);
        const engine = this.engine;
        if (engine === null) {
            this.settings = null;
            return;
        }
        this.timer = setTimeout(() => {
            const asked = ++this.asked;
            engine.settings(this.theory, this.flags).then(
                settings => {
                    if (asked === this.asked) this.settings = settings;
                },
                () => {
                    if (asked === this.asked) this.settings = null;
                },
            );
        }, SETTLE);
    }

    // The values in effect of [flag], and where they come from
    private effective(flag: Pick<Flag, 'name'>): { values: readonly string[]; source: Source } {
        const fromTheory = this.stated.theory.get(flag.name);
        const fromFlags = this.stated.flags.get(flag.name);
        const fallback = DEFAULTS[flag.name] ?? [];
        // an engine telling a value that stands for several (the builtin of an older solver) tells those
        const values = expanded(flag.name, this.settings?.[flag.name] ?? fromTheory ?? fromFlags ?? fallback);
        const same = (a: readonly string[], b: readonly string[]) => a.length === b.length && [...a].sort().every((it, i) => it === [...b].sort()[i]);
        const source: Source =
            fromTheory !== undefined ? 'theory'
            : fromFlags !== undefined && same(values, fromFlags) ? 'flags'
            : same(values, fallback) ? 'default'
            : 'profile';
        return { values, source };
    }

    private profile(): { name: string; source: Source } {
        const name = profileOf(this.theory, this.flags, this.settings);
        return { name, source: statedProfile(this.theory) !== null ? 'theory' : statedProfile(this.flags) !== null ? 'flags' : 'default' };
    }

    private change(flags: string): void {
        this.flags = flags;
        emit(this, 'arg2p-flags-change', { flags });
    }

    private set(flag: Flag, values: readonly string[]): void {
        if (flag.kind === 'switch') this.change(withSwitch(this.flags, flag.name, values.length > 0));
        else if (flag.kind === 'set' && values.length === 0) this.change(withNone(this.flags, flag.name));
        else if (values.length === 0) this.change(without(this.flags, flag.name));
        else this.change(withValues(this.flags, flag.name, values));
    }

    // What the panel finds wrong with the flags in effect
    private warnings(): string[] {
        const warnings = FLAGS.filter(flag => flag.required && this.effective(flag).values.length === 0).map(flag => `${flag.label} has no value: the solver needs one.`);
        const value = (name: string) => this.effective({ name }).values[0];
        if (value('orderingComparator') === 'normal' && value('orderingPrinciple') === 'weakest') {
            warnings.push('The normal comparator goes with the last link alone: with the weakest link the evaluation raises an error.');
        }
        return warnings;
    }

    protected override render() {
        const profile = this.profile();
        const info = profileInfo(profile.name);
        const relevant = FLAG_GROUPS.filter(group => info === undefined || info.groups.includes(group.id));
        const others = FLAG_GROUPS.filter(group => !relevant.includes(group));
        const warnings = this.warnings();
        return html`
            <section class="profile">
                <label class="row">
                    <span class="name">Profile</span>
                    <select
                        data-value=${profile.name}
                        ?disabled=${profile.source === 'theory'}
                        @change=${(e: Event) => this.change(withValues(this.flags, 'graphBuildMode', [(e.target as HTMLSelectElement).value]))}
                    >
                        ${PROFILES.map(it => html`<option value=${it.name} ?selected=${it.name === profile.name}>${it.label}</option>`)}
                        ${info === undefined ? html`<option selected value=${profile.name}>${profile.name}</option>` : nothing}
                    </select>
                    ${this.badge(profile.source)}
                </label>
                <p class="help">
                    ${info?.help ?? 'A profile this version of the panel does not know.'}
                    ${profile.source === 'theory' ? html`<br /><em>The theory states it: change it there.</em>` : nothing}
                </p>
            </section>
            ${warnings.length > 0 ? html`<ul class="warnings" role="alert">${warnings.map(it => html`<li>${it}</li>`)}</ul>` : nothing}
            ${relevant.map(group => this.group(group))}
            ${others.length > 0
                ? html`
                      <details class="others">
                          <summary>Other flags <span class="muted">(${others.map(it => it.title.toLowerCase()).join(', ')}: preset by the profile)</span></summary>
                          ${others.map(group => this.group(group))}
                      </details>
                  `
                : nothing}
        `;
    }

    private group(group: FlagGroup) {
        return html`
            <fieldset class="group">
                <legend>${group.title}</legend>
                ${group.flags.map(flag => this.flag(flag))}
            </fieldset>
        `;
    }

    private flag(flag: Flag) {
        const { values, source } = this.effective(flag);
        const locked = source === 'theory';
        const stated = this.stated.flags.has(flag.name);
        const id = `flag-${flag.name}`;
        let control;
        if (flag.kind === 'switch') {
            control = html`<input id=${id} type="checkbox" .checked=${values.length > 0} ?disabled=${locked} @change=${(e: Event) => this.set(flag, (e.target as HTMLInputElement).checked ? ['true'] : [])} />`;
        } else if (flag.kind === 'choice') {
            control = html`
                <select id=${id} data-value=${values[0] ?? ''} ?disabled=${locked} @change=${(e: Event) => this.set(flag, [(e.target as HTMLSelectElement).value])}>
                    ${values.length === 0 ? html`<option selected disabled value="">none</option>` : nothing}
                    ${flag.options!.map(it => html`<option value=${it.value} title=${it.help} ?selected=${values[0] === it.value}>${it.label}</option>`)}
                </select>
            `;
        } else if (flag.kind === 'set') {
            control = html`
                <div class="set" id=${id} role="group" aria-label=${flag.label}>
                    ${flag.options!.map(
                        it => html`
                            <label title=${it.help}>
                                <input
                                    type="checkbox"
                                    value=${it.value}
                                    .checked=${values.includes(it.value)}
                                    ?disabled=${locked}
                                    @change=${(e: Event) => {
                                        const on = (e.target as HTMLInputElement).checked;
                                        this.set(flag, on ? [...values.filter(v => v !== it.value), it.value] : values.filter(v => v !== it.value));
                                    }}
                                />
                                ${it.label}
                            </label>
                        `,
                    )}
                </div>
            `;
        } else {
            control = html`<input id=${id} type="text" placeholder=${flag.placeholder ?? ''} .value=${values[0] ?? ''} ?disabled=${locked} @change=${(e: Event) => this.set(flag, [(e.target as HTMLInputElement).value])} />`;
        }
        const option = flag.options?.find(it => it.value === values[0]);
        return html`
            <div class="flag ${flag.kind}" data-flag=${flag.name}>
                <div class="row">
                    <label class="name" for=${id} title=${flag.help}>${flag.label}</label>
                    ${flag.kind === 'set' ? nothing : control}
                    ${this.badge(source)}
                    ${stated && !locked ? html`<button type="button" class="reset" title="Take the value of the profile again (remove it from the flags)" @click=${() => this.change(without(this.flags, flag.name))}>reset</button>` : nothing}
                </div>
                ${flag.kind === 'set' ? control : nothing}
                <p class="help">
                    ${flag.kind === 'choice' && option !== undefined ? option.help : flag.help}
                    ${stated && source === 'profile' ? html`<br /><em>The flags state its default, which the profile overrides: state it in the theory to keep it.</em>` : nothing}
                </p>
            </div>
        `;
    }

    private badge(source: Source) {
        const titles: Record<Source, string> = {
            theory: 'Stated by the theory',
            flags: 'Stated by the flags',
            profile: 'Preset by the profile',
            default: 'The default',
        };
        return html`<span class="badge ${source}" title=${titles[source]}>${source}</span>`;
    }

    static override styles = [
        tokens,
        css`
            :host {
                display: block;
                font-size: 13px;
            }
            .profile {
                padding: 8px 0;
            }
            .row {
                display: flex;
                align-items: center;
                gap: 8px;
            }
            /* a flag: its name, its control, where its value comes from, and the reset */
            .flag .row {
                display: grid;
                grid-template-columns: minmax(7em, 10em) minmax(0, 1fr) auto auto;
            }
            .flag.set .row {
                grid-template-columns: minmax(7em, 10em) auto auto 1fr;
            }
            .flag .row > select,
            .flag .row > input[type='text'] {
                min-width: 0;
                width: 100%;
            }
            .flag .row > input[type='checkbox'] {
                justify-self: start;
                margin: 0;
            }
            .name {
                font-weight: 600;
            }
            .profile select {
                flex: 1;
                min-width: 0;
            }
            select,
            input[type='text'] {
                font: inherit;
                color: var(--_text);
                background: var(--_surface);
                border: 1px solid var(--_border);
                border-radius: 4px;
                padding: 2px 4px;
            }
            input[type='text'] {
                font-family: var(--_mono);
            }
            select:disabled,
            input:disabled {
                opacity: 0.7;
            }
            .help {
                margin: 2px 0 0 0;
                color: var(--_muted);
                font-size: 12px;
            }
            .group {
                border: 1px solid var(--_border);
                border-radius: 6px;
                margin: 8px 0;
                padding: 4px 10px 8px 10px;
            }
            legend {
                font-weight: 600;
                padding: 0 4px;
            }
            .flag {
                padding: 6px 0;
            }
            .flag + .flag {
                border-top: 1px dashed var(--_border);
            }
            .set {
                display: flex;
                flex-wrap: wrap;
                gap: 4px 14px;
                margin-top: 4px;
            }
            .set label {
                display: inline-flex;
                align-items: center;
                gap: 4px;
            }
            .badge {
                font-size: 10px;
                text-transform: uppercase;
                letter-spacing: 0.04em;
                border-radius: 4px;
                padding: 0 5px;
                border: 1px solid var(--_border);
                color: var(--_muted);
            }
            .badge.flags {
                color: var(--_accent);
                border-color: var(--_accent);
            }
            .badge.theory {
                color: var(--_surface);
                background: var(--_accent);
                border-color: var(--_accent);
            }
            .badge.profile {
                border-style: dashed;
            }
            .reset {
                font: inherit;
                font-size: 11px;
                color: var(--_muted);
                background: none;
                border: none;
                text-decoration: underline;
                cursor: pointer;
                padding: 0;
            }
            .warnings {
                margin: 4px 0;
                padding: 6px 10px 6px 24px;
                border-left: 3px solid var(--_no);
                background: color-mix(in srgb, var(--_no) 12%, transparent);
                border-radius: 4px;
            }
            .others summary {
                cursor: pointer;
                padding: 6px 0;
            }
            .muted {
                color: var(--_muted);
                font-size: 12px;
            }
        `,
    ];
}

define('arg2p-flags', Arg2pFlags);

declare global {
    interface HTMLElementTagNameMap {
        'arg2p-flags': Arg2pFlags;
    }
}
