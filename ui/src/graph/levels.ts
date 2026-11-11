import type { ArgumentData } from '../engine.js';
import type { Link } from './layout.js';

/** The supports between the arguments of a graph: every argument from each of those it is built on */
export function supportsOf(argumentList: readonly ArgumentData[]): Link[] {
    const ids = new Set(argumentList.map(it => it.id));
    return argumentList.flatMap(it => it.supports.filter(sub => ids.has(sub)).map(sub => ({ from: sub, to: it.id })));
}

/** The row of every argument in a layered drawing: facts on row 0, an argument one row above the highest it is built on */
export function levelsOf(argumentList: readonly ArgumentData[]): Map<string, number> {
    const supports = new Map(argumentList.map(it => [it.id, it.supports]));
    const levels = new Map<string, number>();
    const levelOf = (id: string, visiting: Set<string>): number => {
        if (levels.has(id)) return levels.get(id)!;
        // sub-arguments missing from the graph, or (defensively) a cycle, do not raise the level
        if (!supports.has(id) || visiting.has(id)) return -1;
        visiting.add(id);
        const level = 1 + Math.max(-1, ...supports.get(id)!.map(sub => levelOf(sub, visiting)));
        visiting.delete(id);
        levels.set(id, level);
        return level;
    };
    argumentList.forEach(it => levelOf(it.id, new Set()));
    return levels;
}
