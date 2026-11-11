import type { GraphData } from '../engine.js';
import { nameOf } from '../graph/drawing.js';
import { isAtom } from './atoms.js';
import { redrawn } from './declarations.js';

// Abstract argumentation frameworks in the formats of the ICCMA competitions, to exchange them with other tools:
//
//   APX        arg(a).  att(a,b).
//   TGF        one argument per line, a line `#`, then one attack per line: `a b`
//   ICCMA'23   `p af N`, then attacks between the arguments 1..N: `1 2`
//
// A framework is read into a theory of the profile abstract (`a.`, `a -> b.`), the way arg2p reads Dung frameworks,
// and written from the graph of one: whatever clauses derived its arguments and attacks.

export interface Framework {
    readonly arguments: readonly string[];
    readonly attacks: readonly { readonly from: string; readonly to: string }[];
}

/** A framework read, with the names made atoms: `renamed` lists those that changed (`1` -> `a1`) */
export interface ReadFramework extends Framework {
    readonly renamed: readonly { readonly from: string; readonly to: string }[];
}

/** The theory of [framework], in the profile abstract: a new theory declaring it */
export function toTheory(framework: Framework): string {
    return redrawn('', framework);
}

/** The framework of [graph], an abstract one: its arguments by the names the theory gives them */
export function frameworkOf(graph: GraphData): Framework {
    const names = new Map(graph.arguments.map(it => [it.id, nameOf(it, true)]));
    return {
        arguments: [...names.values()],
        attacks: graph.attacks
            .filter(it => names.has(it.from) && names.has(it.to))
            .map(it => ({ from: names.get(it.from)!, to: names.get(it.to)! })),
    };
}

export function toApx(framework: Framework): string {
    return [...framework.arguments.map(it => `arg(${it}).`), ...framework.attacks.map(it => `att(${it.from},${it.to}).`)].join('\n') + '\n';
}

export function toTgf(framework: Framework): string {
    return [...framework.arguments, '#', ...framework.attacks.map(it => `${it.from} ${it.to}`)].join('\n') + '\n';
}

/** A framework in any of the three formats (an Error says where it is not) */
export function parseFramework(text: string): ReadFramework {
    const lines = text.split(/\r?\n/).map(it => it.replace(/%.*$/, '').trim());
    const content = lines.filter(it => (it !== '' && !it.startsWith('#')) || it === '#');
    if (content.some(it => /^(arg|att)\s*\(/.test(it))) return named(parseApx(content));
    if (content.length > 0 && /^p\s+af\s+\d+$/.test(content[0])) return named(parseIccma23(content));
    return named(parseTgf(content));
}

function parseApx(lines: string[]): Framework {
    const args: string[] = [];
    const attacks: { from: string; to: string }[] = [];
    lines.forEach((line, index) => {
        line.split(/(?<=\))\s*\.\s*/)
            .filter(it => it.trim() !== '')
            .forEach(fact => {
                const arg = /^arg\s*\(\s*([^,()]+?)\s*\)\s*\.?$/.exec(fact);
                const att = /^att\s*\(\s*([^,()]+?)\s*,\s*([^,()]+?)\s*\)\s*\.?$/.exec(fact);
                if (arg) args.push(arg[1]);
                else if (att) attacks.push({ from: att[1], to: att[2] });
                else throw new Error(`Line ${index + 1}: expected arg(a). or att(a,b)., found "${fact}"`);
            });
    });
    return { arguments: args, attacks };
}

function parseTgf(lines: string[]): Framework {
    const separator = lines.indexOf('#');
    if (separator === -1) {
        throw new Error("Not an APX, TGF or ICCMA'23 framework: a TGF file has a line # between arguments and attacks");
    }
    // a node line may carry a label after the id: the id is the name
    const args = lines.slice(0, separator).map(it => it.split(/\s+/)[0]);
    const attacks = lines
        .slice(separator + 1)
        .filter(it => it !== '#')
        .map((line, index) => {
            const parts = line.split(/\s+/);
            if (parts.length < 2) throw new Error(`Attack ${index + 1}: expected two arguments, found "${line}"`);
            return { from: parts[0], to: parts[1] };
        });
    return { arguments: args, attacks };
}

function parseIccma23(lines: string[]): Framework {
    const count = parseInt(lines[0].split(/\s+/)[2]);
    const args = Array.from({ length: count }, (_, i) => `${i + 1}`);
    const attacks = lines.slice(1).map((line, index) => {
        const parts = line.split(/\s+/);
        if (parts.length !== 2 || parts.some(it => !/^\d+$/.test(it) || +it < 1 || +it > count)) {
            throw new Error(`Attack ${index + 1}: expected two arguments between 1 and ${count}, found "${line}"`);
        }
        return { from: parts[0], to: parts[1] };
    });
    return { arguments: args, attacks };
}

// Every name made an atom, the same way everywhere it appears, and the arguments only mentioned by attacks added
function named(framework: Framework): ReadFramework {
    const names = new Map<string, string>();
    const used = new Set(framework.arguments.filter(isAtom));
    const atom = (raw: string): string => {
        const known = names.get(raw);
        if (known !== undefined) return known;
        let name = isAtom(raw) ? raw : candidate(raw);
        if (name !== raw) {
            const base = name;
            for (let n = 2; used.has(name); n++) name = `${base}_${n}`;
            used.add(name);
        }
        names.set(raw, name);
        return name;
    };
    const args: string[] = [];
    const add = (name: string) => {
        if (!args.includes(name)) args.push(name);
    };
    framework.arguments.forEach(it => add(atom(it)));
    const attacks: { from: string; to: string }[] = [];
    framework.attacks.forEach(it => {
        const attack = { from: atom(it.from), to: atom(it.to) };
        add(attack.from);
        add(attack.to);
        if (!attacks.some(a => a.from === attack.from && a.to === attack.to)) attacks.push(attack);
    });
    const renamed = [...names].filter(([raw, name]) => raw !== name).map(([from, to]) => ({ from, to }));
    return { arguments: args, attacks, renamed };
}

function candidate(raw: string): string {
    const cleaned = raw.replace(/[^a-zA-Z0-9_]/g, '_');
    if (/^[a-z]/.test(cleaned)) return cleaned;
    if (/^[A-Z]/.test(cleaned)) return cleaned[0].toLowerCase() + cleaned.slice(1);
    return `a${cleaned}`;
}
