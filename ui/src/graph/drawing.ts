import type { ArgumentData, AttackData, GraphData } from '../engine.js';

// What a graph is drawn from, apart from the canvas that draws it: the names of the arguments, what their nodes say,
// and how their edges bend.

// Conclusions on the nodes are wrapped at their commas into lines of about this length, and cut after a few lines:
// the popup has the full text
const CONCLUSION_LINE = 18;
const CONCLUSION_LINES = 3;
// Attacks are curved, so that mutual attacks do not overlap; long ones are curved less, or they would arc across the
// whole drawing
const CURVE = 0.25;
const CURVE_LENGTH = 160;
const MIN_CURVE = 0.06;

/** The id of the edge of an attack from [from] to [to], `a->b`: by it the attack, and the one back, are found */
export function attackId(from: string, to: string): string {
    return `${from}->${to}`;
}

/** Whether the arguments of [graph] are those of an abstract framework: named by the theory, with no structure */
export function isAbstract(graph: GraphData): boolean {
    return graph.profile === 'abstract';
}

/** The name [argument] is shown by: its identifier (A0, ...), or in an abstract framework the name the theory gives it */
export function nameOf(argument: ArgumentData, abstract: boolean): string {
    return abstract ? argument.conclusion : argument.id;
}

/** What the node of [argument] says: its name, and its conclusion under it when [conclusions] are shown */
export function nodeLabel(argument: ArgumentData, abstract: boolean, conclusions: boolean): string {
    const name = nameOf(argument, abstract);
    // an abstract argument is its conclusion
    if (abstract || !conclusions) return name;
    return [name, ...wrapTerm(argument.conclusion)].join('\n');
}

/** `caFa(closedReg, damages)` -> ["caFa(closedReg,", "damages)"]: lines broken after commas, the last cut short */
export function wrapTerm(term: string): string[] {
    const lines: string[] = [];
    term.split(/(?<=,) /).forEach(part => {
        const last = lines.length - 1;
        if (last >= 0 && `${lines[last]} ${part}`.length <= CONCLUSION_LINE) lines[last] += ` ${part}`;
        else lines.push(part);
    });
    if (lines.length <= CONCLUSION_LINES) return lines;
    return [...lines.slice(0, CONCLUSION_LINES - 1), `${lines[CONCLUSION_LINES - 1]} …`];
}

/** The roundness of an attack from [from] to [to]: less for long ones */
export function curveOf(from: { x: number; y: number }, to: { x: number; y: number }): number {
    const length = Math.hypot(to.x - from.x, to.y - from.y);
    if (length === 0) return CURVE;
    return Math.max(MIN_CURVE, Math.min(CURVE, (CURVE * CURVE_LENGTH) / length));
}

/**
 * The statement of the theory [argument] comes from: the rule it is built by last, or for a premise (an axiom, an
 * ordinary premise) the name the theory gives it, or the argument itself in an abstract framework. By it a page links
 * the argument to what the statement is about (the passage of a text it formalizes, say).
 */
export function statementOf(argument: ArgumentData, abstract: boolean): string {
    if (abstract) return argument.conclusion;
    if (argument.topRule !== 'none') return argument.topRule;
    // `A0 : f1 : bird(tweety)`: the premise is named between the identifier and the conclusion
    return argument.descriptor.split(' : ')[1]?.trim() ?? argument.id;
}

/** The statement [statement] stands for: itself, or the rule a transposed one comes from (`r1_i`, `r1_i2` -> `r1`) */
export function sourceStatement(statement: string): string {
    return statement.replace(/_i\d*$/, '');
}

/** Whether [statement] is among [linked], itself or as the rule it is a transposition of */
/** The passages [passages] gives for [statement], or for the rule it is transposed from */
export function passagesOf(passages: Readonly<Record<string, readonly string[]>>, statement: string): readonly string[] {
    return passages[statement] ?? passages[sourceStatement(statement)] ?? [];
}

export function isLinked(statement: string, linked: ReadonlySet<string>): boolean {
    return linked.has(statement) || linked.has(sourceStatement(statement));
}

/** The kind of [argument]: an assumption (ABA), a premise (no top rule), or one built by its top rule */
export function kindOf(argument: ArgumentData, abstract: boolean): 'abstract' | 'assumption' | 'premise' | 'rule' {
    if (abstract) return 'abstract';
    if (/ : asm\(/.test(argument.descriptor)) return 'assumption';
    return argument.topRule === 'none' ? 'premise' : 'rule';
}

/** The attacks [graph] has on and by [id], with the arguments they come from or go to */
export function attacksAround(graph: GraphData, id: string): { by: AttackData[]; on: AttackData[] } {
    return {
        by: graph.attacks.filter(it => it.to === id),
        on: graph.attacks.filter(it => it.from === id),
    };
}
