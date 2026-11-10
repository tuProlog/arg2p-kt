import { quote, unquote } from '../formats/atoms.js';
import { DEFAULT_PROFILE, FLAGS, type Flag, type Settings } from './schema.js';

// The flags as a text of Prolog facts, `name(value).` or a plain `name.` switch: the text stays the source of truth,
// and a change touches the lines of its flag alone (the others, and comments, stay as they are).

/** What a text states for a flag: its values (for a switch, `true` if it is there), or nothing when it does not state it */
export type Stated = ReadonlyMap<string, readonly string[]>;

function escape(name: string): string {
    return name.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

// A line stating the flag with a value, `name(value).`, and an optional comment after it
function valueLine(name: string): RegExp {
    return new RegExp(`^\\s*${escape(name)}\\s*\\(\\s*(.*?)\\s*\\)\\s*\\.\\s*(%.*)?$`);
}

function switchLine(name: string): RegExp {
    return new RegExp(`^\\s*${escape(name)}\\s*\\.\\s*(%.*)?$`);
}

function valuesOf(text: string, name: string): string[] {
    const pattern = valueLine(name);
    return text
        .split('\n')
        .map(line => pattern.exec(line))
        .filter((match): match is RegExpExecArray => match !== null)
        .map(match => unquote(match[1]));
}

/**
 * The values of the flag [name] with those standing for several of its options (its aliases, as the `builtin` of
 * conflictModel) replaced by these, each once, in the order they come
 */
export function expanded(name: string, values: readonly string[], flags: readonly Flag[] = FLAGS): readonly string[] {
    const aliases = flags.find(it => it.name === name)?.aliases;
    if (aliases === undefined) return values;
    return [...new Set(values.flatMap(it => aliases[it] ?? [it]))];
}

/**
 * The flags [text] states, of [flags]: a switch stated on has the value `true` (`name.`), off none (`name(off).`); a
 * flag stated with no value (`name(none).`) has none; a value standing for several (an alias) states those
 */
export function statedFlags(text: string, flags: readonly Flag[] = FLAGS): Stated {
    const stated = new Map<string, readonly string[]>();
    flags.forEach(flag => {
        const values = valuesOf(text, flag.name);
        if (flag.kind === 'switch') {
            if (text.split('\n').some(line => switchLine(flag.name).test(line))) stated.set(flag.name, ['true']);
            else if (values.length > 0) stated.set(flag.name, values.includes('off') ? [] : ['true']);
        } else if (values.length > 0) {
            stated.set(flag.name, expanded(flag.name, values.filter(it => it !== 'none'), flags));
        }
    });
    return stated;
}

/** The profile [text] states (graphBuildMode), if any */
export function statedProfile(text: string): string | null {
    return valuesOf(text, 'graphBuildMode')[0] ?? null;
}

/**
 * The profile [theory] is read in, solved with [flags]: the one in effect, as [settings] (those an engine tells) have
 * it, or else the one the theory states, or the flags, or the default
 */
export function profileOf(theory: string, flags: string, settings: Settings | null = null): string {
    return settings?.graphBuildMode?.[0] ?? statedProfile(theory) ?? statedProfile(flags) ?? DEFAULT_PROFILE;
}

// Every line matching [pattern] replaced by [replacement] (some lines, or none), where the first of them was, or at
// the end when there was none
function replaceLines(text: string, patterns: readonly RegExp[], replacement: readonly string[]): string {
    const matches = (line: string) => patterns.some(it => it.test(line));
    const lines = text.split('\n');
    const first = lines.findIndex(matches);
    const kept = lines.filter(line => !matches(line));
    if (first === -1) {
        if (replacement.length === 0) return text;
        const body = text.replace(/\s*$/, '');
        return `${body === '' ? '' : `${body}\n`}${replacement.join('\n')}\n`;
    }
    kept.splice(first, 0, ...replacement);
    return kept.join('\n');
}

/** [text] stating [values] for the flag [name] (a choice or a text has one, or none: empty), one line each */
export function withValues(text: string, name: string, values: readonly string[]): string {
    const present = values.map(it => it.trim()).filter(it => it !== '');
    return replaceLines(text, [valueLine(name)], present.map(it => `${name}(${quote(it)}).`));
}

/** [text] stating a set with no value (`name(none).`): it clears what a profile presets */
export function withNone(text: string, name: string): string {
    return replaceLines(text, [valueLine(name)], [`${name}(none).`]);
}

/** [text] stating the switch [name] on (`name.`) or off (`name(off).`) */
export function withSwitch(text: string, name: string, on: boolean): string {
    return replaceLines(text, [switchLine(name), valueLine(name)], [on ? `${name}.` : `${name}(off).`]);
}

/** [text] stating nothing for the flag [name]: it takes the value of the profile, or its default */
export function without(text: string, name: string): string {
    return replaceLines(text, [switchLine(name), valueLine(name)], []);
}
