// @vitest-environment happy-dom
import { afterEach, assert, describe, it } from 'vitest';
import '../src/index.js';
import type { Arg2pFlags } from '../src/components/arg2p-flags.js';
import { CONFLICT_PARTS, DEFAULT_FLAGS, DEFAULTS, FLAGS, type Settings } from '../src/flags/schema.js';
import { expanded, profileOf, statedFlags, statedProfile, withNone, withSwitch, withValues, without } from '../src/flags/text.js';

const { deepEqual, equal } = assert;
const ok: (value: unknown, message?: string) => void = assert.ok;

// flags stating every flag that needs a value
const FULL = [
    'graphBuildMode(aspic_plus).',
    'argumentLabellingMode(grounded).',
    'statementLabellingMode(statement).',
    'orderingPrinciple(last).',
    'orderingComparator(elitist).',
    'graphExtension(standardPref).',
    'queryMode.',
    '',
].join('\n');

describe('Flags as text', function () {
    it('should read every kind of flag', function () {
        const stated = statedFlags(`${FULL}graphExtension(bp).\nmodulesPath('https://example.org/t').\nautoTransposition(off).\n`);
        deepEqual(stated.get('argumentLabellingMode'), ['grounded']);
        deepEqual(stated.get('graphExtension'), ['standardPref', 'bp']);
        deepEqual(stated.get('queryMode'), ['true']);
        deepEqual(stated.get('autoTransposition'), []);
        deepEqual(stated.get('modulesPath'), ['https://example.org/t']);
        equal(stated.has('metaRules'), false);
        equal(statedProfile(FULL), 'aspic_plus');
        equal(statedProfile('queryMode.'), null);
    });

    it('should tell the profile in effect: as the engine has it, or as the theory, the flags or the default state it', function () {
        equal(profileOf('graphBuildMode(aba).', 'graphBuildMode(abstract).'), 'aba');
        equal(profileOf('a.', 'graphBuildMode(abstract).'), 'abstract');
        equal(profileOf('a.', 'queryMode.'), 'aspic_plus');
        equal(profileOf('graphBuildMode(aba).', '', { graphBuildMode: ['abstract'] }), 'abstract');
        equal(profileOf('graphBuildMode(aba).', '', {}), 'aba');
    });

    it('should tolerate spaces and comments, and read a set stated empty', function () {
        const stated = statedFlags('  argumentLabellingMode( preferred ) . % the semantics\n queryMode . % fast\ngraphExtension(none).');
        deepEqual(stated.get('argumentLabellingMode'), ['preferred']);
        deepEqual(stated.get('queryMode'), ['true']);
        deepEqual(stated.get('graphExtension'), []);
    });

    it('should change a value in place, and add a missing one at the end', function () {
        const text = withValues(FULL, 'argumentLabellingMode', ['stable']);
        equal(text.split('\n')[1], 'argumentLabellingMode(stable).');
        equal(text.split('\n').length, FULL.split('\n').length);
        equal(withValues('queryMode.', 'modulesPath', ['https://example.org/t']), "queryMode.\nmodulesPath('https://example.org/t').\n");
    });

    it('should set the values of a set where the first one was, or none', function () {
        const text = withValues(FULL, 'graphExtension', ['standardPref', 'rebutRestriction']);
        deepEqual(statedFlags(text).get('graphExtension'), ['standardPref', 'rebutRestriction']);
        equal(text.split('\n')[5], 'graphExtension(standardPref).');
        equal(text.split('\n')[6], 'graphExtension(rebutRestriction).');
        deepEqual(statedFlags(withNone(text, 'graphExtension')).get('graphExtension'), []);
    });

    it('should read the conflict model as its parts, builtin as all of them', function () {
        deepEqual(statedFlags('conflictModel(builtin).').get('conflictModel'), CONFLICT_PARTS);
        deepEqual(statedFlags('conflictModel(declared).').get('conflictModel'), ['declared']);
        deepEqual(statedFlags('conflictModel(none).').get('conflictModel'), []);
        deepEqual(statedFlags('conflictModel(negation).\nconflictModel(declared).').get('conflictModel'), ['negation', 'declared']);
        // each part once, in the order stated
        deepEqual(statedFlags('conflictModel(declared).\nconflictModel(builtin).').get('conflictModel'), ['declared', 'negation', 'deontic', 'preference']);
        deepEqual(expanded('conflictModel', ['builtin']), CONFLICT_PARTS);
        // a flag with no aliases keeps its values
        deepEqual(expanded('graphExtension', ['builtin']), ['builtin']);
    });

    it('should turn switches on and off, and forget a flag', function () {
        const on = withSwitch(FULL, 'autoTransposition', true);
        deepEqual(statedFlags(on).get('autoTransposition'), ['true']);
        const off = withSwitch(on, 'queryMode', false);
        ok(off.includes('queryMode(off).'));
        ok(!off.includes('queryMode.'));
        equal(statedFlags(without(off, 'queryMode')).has('queryMode'), false);
    });

    it('should keep the lines it does not know', function () {
        const text = withValues(`% my flags\ncustomFlag(x).\n${FULL}`, 'orderingPrinciple', ['weakest']);
        ok(text.startsWith('% my flags\ncustomFlag(x).\n'));
    });
});

describe('<arg2p-flags>', function () {
    // An engine that answers with the settings of [answer], and counts what it was asked
    function engine(answer: (theory: string, flags: string) => Settings) {
        const asked: { theory: string; flags: string }[] = [];
        return {
            asked,
            settings(theory: string, flags: string) {
                asked.push({ theory, flags });
                return Promise.resolve(answer(theory, flags));
            },
        };
    }

    const ABA: Settings = { ...DEFAULTS, graphBuildMode: ['aba'], graphExtension: [], conflictModel: ['declared'], graphLanguage: ['abaLanguage'] };

    async function panel(flags: string, theory: string, settings: (theory: string, flags: string) => Settings) {
        const element = document.createElement('arg2p-flags') as Arg2pFlags;
        const answering = engine(settings);
        element.flags = flags;
        element.theory = theory;
        element.engine = answering;
        document.body.appendChild(element);
        await element.updateComplete;
        // the settings are asked once the changes settle
        await new Promise(resolve => setTimeout(resolve, 300));
        await element.updateComplete;
        const changes: string[] = [];
        element.addEventListener('arg2p-flags-change', event => changes.push((event as CustomEvent).detail.flags));
        return { element, answering, changes, root: element.shadowRoot! };
    }

    // A flag as the panel shows it: the values of its control, and where they come from (its badge)
    function shownFlag(root: ShadowRoot, name: string): { values: string[]; source: string } {
        const flag = root.querySelector(`[data-flag=${name}]`)!;
        const source = flag.querySelector('.badge')!.textContent!;
        const select = flag.querySelector('select');
        if (select !== null) return { values: select.value === '' ? [] : [select.value], source };
        const boxes = [...flag.querySelectorAll<HTMLInputElement>('input[type=checkbox]')];
        if (flag.classList.contains('switch')) return { values: boxes[0].checked ? ['true'] : [], source };
        if (boxes.length > 0) return { values: boxes.filter(it => it.checked).map(it => it.value), source };
        return { values: [flag.querySelector('input')!.value].filter(it => it !== ''), source };
    }

    afterEach(() => {
        document.body.replaceChildren();
    });

    it('should show the values in effect, and where they come from', async function () {
        const { root } = await panel(`graphBuildMode(aba).\n${DEFAULT_FLAGS}`, 'r1 : a -> b.', () => ABA);
        equal(root.querySelector<HTMLSelectElement>('.profile select')!.value, 'aba');
        deepEqual(shownFlag(root, 'graphExtension'), { values: [], source: 'profile' });
        deepEqual(shownFlag(root, 'argumentLabellingMode'), { values: ['grounded'], source: 'flags' });
        deepEqual(shownFlag(root, 'conflictModel'), { values: ['declared'], source: 'profile' });
        deepEqual(shownFlag(root, 'metaRules'), { values: [], source: 'default' });
        // ABA has no preferences: they are folded away
        const shown = [...root.children].filter(it => it.tagName === 'FIELDSET').map(it => it.querySelector('legend')!.textContent);
        deepEqual(shown, ['Semantics', 'Evaluation', 'Framework']);
        ok(root.querySelector('details.others')!.textContent!.includes('Preferences'));
    });

    it('should leave to the theory what the theory states', async function () {
        const { root } = await panel(DEFAULT_FLAGS, 'graphBuildMode(abstract).\nargumentLabellingMode(preferred).\na -> b.', () => ({
            ...DEFAULTS,
            graphBuildMode: ['abstract'],
            argumentLabellingMode: ['preferred'],
        }));
        ok(root.querySelector<HTMLSelectElement>('.profile select')!.disabled);
        deepEqual(shownFlag(root, 'argumentLabellingMode'), { values: ['preferred'], source: 'theory' });
        ok(root.querySelector<HTMLSelectElement>('#flag-argumentLabellingMode')!.disabled);
    });

    it('should write the changes into the flags, a line each', async function () {
        const { root, changes, answering } = await panel(DEFAULT_FLAGS, '', () => DEFAULTS);
        const profile = root.querySelector<HTMLSelectElement>('.profile select')!;
        profile.value = 'abstract';
        profile.dispatchEvent(new Event('change'));
        equal(changes.at(-1), `${DEFAULT_FLAGS}graphBuildMode(abstract).\n`);

        const semantics = root.querySelector<HTMLSelectElement>('#flag-argumentLabellingMode')!;
        semantics.value = 'preferred';
        semantics.dispatchEvent(new Event('change'));
        ok(changes.at(-1)!.includes('argumentLabellingMode(preferred).'));

        const restricted = root.querySelector<HTMLInputElement>('input[value=rebutRestriction]')!;
        restricted.checked = true;
        restricted.dispatchEvent(new Event('change'));
        deepEqual(statedFlags(changes.at(-1)!).get('graphExtension'), ['standardPref', 'rebutRestriction']);

        const transposition = root.querySelector<HTMLInputElement>('#flag-autoTransposition')!;
        transposition.checked = true;
        transposition.dispatchEvent(new Event('change'));
        ok(changes.at(-1)!.includes('autoTransposition.'));

        // and asks what is in effect, once
        await new Promise(resolve => setTimeout(resolve, 300));
        equal(answering.asked.at(-1)!.flags, changes.at(-1));
    });

    it('should show the conflict model as a checklist of its parts', async function () {
        const conflicts = FLAGS.find(it => it.name === 'conflictModel')!;
        equal(conflicts.kind, 'set');
        deepEqual(conflicts.options!.map(it => it.value), ['negation', 'deontic', 'preference', 'declared']);
        // all of them by default, and with an engine telling builtin (as the solvers before the parts do)
        deepEqual(shownFlag((await panel(DEFAULT_FLAGS, '', () => DEFAULTS)).root, 'conflictModel'), { values: CONFLICT_PARTS, source: 'default' });
        document.body.replaceChildren();
        deepEqual(shownFlag((await panel(DEFAULT_FLAGS, '', () => ({ ...DEFAULTS, conflictModel: ['builtin'] }))).root, 'conflictModel'), {
            values: CONFLICT_PARTS,
            source: 'default',
        });
    });

    it('should read the conflict model of old texts, builtin as all its parts, declared alone', async function () {
        // no engine: what the texts state
        const shown = async (flags: string, theory: string) => {
            const element = document.createElement('arg2p-flags') as Arg2pFlags;
            element.flags = flags;
            element.theory = theory;
            document.body.appendChild(element);
            await element.updateComplete;
            return shownFlag(element.shadowRoot!, 'conflictModel');
        };
        deepEqual(await shown(`${DEFAULT_FLAGS}conflictModel(builtin).\n`, ''), { values: CONFLICT_PARTS, source: 'flags' });
        deepEqual(await shown(`${DEFAULT_FLAGS}conflictModel(declared).\n`, ''), { values: ['declared'], source: 'flags' });
        deepEqual(await shown(DEFAULT_FLAGS, 'conflictModel(builtin).\na :=> b.'), { values: CONFLICT_PARTS, source: 'theory' });
        deepEqual(await shown(DEFAULT_FLAGS, 'conflictModel(declared).\na :=> b.'), { values: ['declared'], source: 'theory' });
    });

    it('should write the parts of the conflict model ticked, never builtin', async function () {
        const { root, changes } = await panel(`${DEFAULT_FLAGS}conflictModel(builtin).\n`, '', () => DEFAULTS);
        const box = (part: string) => root.querySelector<HTMLInputElement>(`[data-flag=conflictModel] input[value=${part}]`)!;
        const untick = (part: string) => {
            box(part).checked = false;
            box(part).dispatchEvent(new Event('change'));
        };
        untick('deontic');
        const written = changes.at(-1)!;
        ok(!written.includes('builtin'), written);
        deepEqual(statedFlags(written).get('conflictModel'), ['negation', 'preference', 'declared']);
        equal(written, `${DEFAULT_FLAGS}conflictModel(negation).\nconflictModel(preference).\nconflictModel(declared).\n`);
        // none ticked: stated with none, the conflicts of the languages alone
        const none = await panel(`${DEFAULT_FLAGS}conflictModel(declared).\n`, '', () => ({ ...DEFAULTS, conflictModel: ['declared'] }));
        const declared = none.root.querySelector<HTMLInputElement>('[data-flag=conflictModel] input[value=declared]')!;
        declared.checked = false;
        declared.dispatchEvent(new Event('change'));
        equal(none.changes.at(-1), `${DEFAULT_FLAGS}conflictModel(none).\n`);
        deepEqual(statedFlags(none.changes.at(-1)!).get('conflictModel'), []);
    });

    it('should reset a flag to the value of the profile', async function () {
        const { root, changes } = await panel(`${DEFAULT_FLAGS}metaRules.\n`, '', () => ({ ...DEFAULTS, metaRules: ['true'] }));
        const reset = root.querySelector<HTMLButtonElement>('[data-flag=metaRules] .reset')!;
        reset.click();
        equal(statedFlags(changes.at(-1)!).has('metaRules'), false);
    });

    it('should warn about what the solver cannot run with', async function () {
        const { root } = await panel(DEFAULT_FLAGS, '', () => ({ ...DEFAULTS, orderingPrinciple: ['weakest'], orderingComparator: ['normal'], argumentLabellingMode: [] }));
        equal(root.querySelectorAll('.warnings li').length, 2);
    });
});
