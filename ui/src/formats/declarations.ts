import { FLAGS } from '../flags/schema.js';
import { attackId } from '../graph/drawing.js';
import { ATOM } from './atoms.js';
import type { Framework } from './frameworks.js';

// The arguments and attacks a theory of the profile abstract declares, as written in it (`a.`, `arg(a).`, `a -> b.`,
// `att(a, b).`, `a <-> b.`), and the theory rewritten for a framework drawn: only those clauses change, every other
// (the profile, the switches of the flags, comments, the clauses that derive attacks) stays as it is.

/** A clause of a theory: where it is, from its first character to its full stop included, and its text */
interface Clause {
    readonly start: number;
    readonly end: number;
    readonly text: string;
}

type Declaration =
    | { readonly kind: 'argument'; readonly name: string; readonly form: 'fact' | 'arg' }
    | { readonly kind: 'attack'; readonly from: string; readonly to: string; readonly form: 'arrow' | 'att' }
    | { readonly kind: 'mutual'; readonly a: string; readonly b: string };

const NAME = `(${ATOM})`;
const ARGUMENT_FACT = new RegExp(`^${NAME}$`);
const ARGUMENT_ARG = new RegExp(`^arg\\s*\\(\\s*${NAME}\\s*\\)$`);
const ATTACK_ARROW = new RegExp(`^${NAME}\\s*->\\s*${NAME}$`);
const ATTACK_ATT = new RegExp(`^att\\s*\\(\\s*${NAME}\\s*,\\s*${NAME}\\s*\\)$`);
const MUTUAL = new RegExp(`^${NAME}\\s*<->\\s*${NAME}$`);
// A fact of one of these names turns a flag on (`queryMode.`): the solver reads it as no argument
const SWITCHES = new Set(FLAGS.filter(it => it.kind === 'switch').map(it => it.name));

/** The framework [theory] declares: its arguments in the order they are met, with those its attacks name */
export function declaredFramework(theory: string): Framework {
    const args: string[] = [];
    const attacks: { from: string; to: string }[] = [];
    const argument = (name: string) => {
        if (!args.includes(name)) args.push(name);
    };
    const attack = (from: string, to: string) => {
        argument(from);
        argument(to);
        if (!attacks.some(it => it.from === from && it.to === to)) attacks.push({ from, to });
    };
    declarations(theory).forEach(({ declaration }) => {
        if (declaration.kind === 'argument') argument(declaration.name);
        else if (declaration.kind === 'attack') attack(declaration.from, declaration.to);
        else {
            attack(declaration.a, declaration.b);
            attack(declaration.b, declaration.a);
        }
    });
    return { arguments: args, attacks };
}

/**
 * [theory] declaring [framework] instead of what it declares: the declarations no longer drawn removed (half a mutual
 * attack left as a plain one), the new ones added after the last declaration, one per line. [renamed] are arguments
 * renamed in the drawing, renamed where they are declared.
 */
export function redrawn(theory: string, framework: Framework, renamed: ReadonlyMap<string, string> = new Map()): string {
    const name = (it: string) => renamed.get(it) ?? it;
    const wantedArguments = new Set(framework.arguments);
    const wantedAttacks = new Set(framework.attacks.map(it => attackId(it.from, it.to)));
    const declaredArguments = new Set<string>();
    const declaredAttacks = new Set<string>();
    const edits: { start: number; end: number; text: string }[] = [];
    let after = -1;

    declarations(theory).forEach(({ clause, declaration }) => {
        after = clause.end;
        let text: string | null;
        if (declaration.kind === 'argument') {
            const argument = name(declaration.name);
            text = wantedArguments.has(argument) && !declaredArguments.has(argument) ? argumentText(argument, declaration.form) : null;
            if (text !== null) declaredArguments.add(argument);
        } else if (declaration.kind === 'attack') {
            const [from, to] = [name(declaration.from), name(declaration.to)];
            text = wantedAttacks.has(attackId(from, to)) && !declaredAttacks.has(attackId(from, to)) ? attackText(from, to, declaration.form) : null;
            if (text !== null) declaredAttacks.add(attackId(from, to));
        } else {
            const [a, b] = [name(declaration.a), name(declaration.b)];
            const ab = wantedAttacks.has(attackId(a, b)) && !declaredAttacks.has(attackId(a, b));
            const ba = wantedAttacks.has(attackId(b, a)) && !declaredAttacks.has(attackId(b, a));
            text = ab && ba ? `${a} <-> ${b}` : ab ? attackText(a, b, 'arrow') : ba ? attackText(b, a, 'arrow') : null;
            if (ab) declaredAttacks.add(attackId(a, b));
            if (ba) declaredAttacks.add(attackId(b, a));
        }
        if (text === null) edits.push(removal(theory, clause));
        else if (`${text}.` !== clause.text) edits.push({ start: clause.start, end: clause.end, text: `${text}.` });
    });

    // an attack declares the arguments it names
    framework.attacks.filter(it => declaredAttacks.has(attackId(it.from, it.to))).forEach(it => declaredArguments.add(it.from).add(it.to));
    const added = [
        ...framework.arguments.filter(it => !declaredArguments.has(it)).map(it => `${it}.`),
        ...framework.attacks.filter(it => !declaredAttacks.has(attackId(it.from, it.to))).map(it => `${it.from} -> ${it.to}.`),
    ];
    if (added.length > 0) {
        if (after >= 0) edits.push({ start: after, end: after, text: added.map(it => `\n${it}`).join('') });
        else {
            // a theory with no declaration yet: they go at its end, under the profile when it is a new theory
            const blank = theory.trim() === '';
            const head = blank ? 'graphBuildMode(abstract).\n\n' : theory.endsWith('\n') ? '' : '\n';
            edits.push({ start: theory.length, end: theory.length, text: `${head}${added.join('\n')}\n` });
        }
    }

    return edits
        .sort((a, b) => b.start - a.start)
        .reduce((text, edit) => text.slice(0, edit.start) + edit.text + text.slice(edit.end), theory);
}

function argumentText(name: string, form: 'fact' | 'arg'): string {
    return form === 'arg' ? `arg(${name})` : name;
}

function attackText(from: string, to: string, form: 'arrow' | 'att'): string {
    return form === 'att' ? `att(${from}, ${to})` : `${from} -> ${to}`;
}

// A clause taken out, with its line when nothing else is on it
function removal(theory: string, clause: Clause): { start: number; end: number; text: string } {
    const lineStart = theory.lastIndexOf('\n', clause.start - 1) + 1;
    const newline = theory.indexOf('\n', clause.end);
    const lineEnd = newline === -1 ? theory.length : newline;
    const alone = theory.slice(lineStart, clause.start).trim() === '' && theory.slice(clause.end, lineEnd).trim() === '';
    if (!alone) return { start: clause.start, end: clause.end, text: '' };
    return { start: lineStart, end: newline === -1 ? lineEnd : lineEnd + 1, text: '' };
}

function declarations(theory: string): { clause: Clause; declaration: Declaration }[] {
    return clausesOf(theory).flatMap(clause => {
        const declaration = declarationOf(clause.text.slice(0, -1).trim());
        return declaration === null ? [] : [{ clause, declaration }];
    });
}

function declarationOf(text: string): Declaration | null {
    let match: RegExpExecArray | null;
    if ((match = ARGUMENT_FACT.exec(text))) return SWITCHES.has(match[1]) ? null : { kind: 'argument', name: match[1], form: 'fact' };
    if ((match = ARGUMENT_ARG.exec(text))) return { kind: 'argument', name: match[1], form: 'arg' };
    if ((match = ATTACK_ARROW.exec(text))) return { kind: 'attack', from: match[1], to: match[2], form: 'arrow' };
    if ((match = ATTACK_ATT.exec(text))) return { kind: 'attack', from: match[1], to: match[2], form: 'att' };
    if ((match = MUTUAL.exec(text))) return { kind: 'mutual', a: match[1], b: match[2] };
    return null;
}

// The clauses of a Prolog text: up to a full stop followed by a blank (or the end), outside comments and quotes. A
// clause starts at its first character that is not blank nor comment; comments inside it are left in its text, so it
// does not match a declaration and is kept as written.
function clausesOf(theory: string): Clause[] {
    const clauses: Clause[] = [];
    let start = -1;
    let i = 0;
    while (i < theory.length) {
        const char = theory[i];
        if (char === '%') {
            const end = theory.indexOf('\n', i);
            i = end === -1 ? theory.length : end;
            continue;
        }
        if (char === '/' && theory[i + 1] === '*') {
            const end = theory.indexOf('*/', i + 2);
            i = end === -1 ? theory.length : end + 2;
            continue;
        }
        if (/\s/.test(char)) {
            i++;
            continue;
        }
        if (start === -1) start = i;
        if (char === "'" || char === '"' || char === '`') {
            i = closing(theory, i);
            continue;
        }
        if (char === '0' && theory[i + 1] === "'") {
            // a character code, 0'c
            i += 3;
            continue;
        }
        if (char === '.' && (i + 1 === theory.length || /[\s%]/.test(theory[i + 1])) && !/[#$&*+\-./:<=>?@^~\\]/.test(theory[i - 1] ?? '')) {
            clauses.push({ start, end: i + 1, text: theory.slice(start, i + 1) });
            start = -1;
        }
        i++;
    }
    return clauses;
}

// Past the quote closing the one at [at] (a doubled quote stands for itself)
function closing(text: string, at: number): number {
    const quote = text[at];
    let i = at + 1;
    while (i < text.length) {
        if (text[i] === '\\') i += 2;
        else if (text[i] === quote && text[i + 1] === quote) i += 2;
        else if (text[i] === quote) return i + 1;
        else i++;
    }
    return text.length;
}
