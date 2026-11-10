import type { CompactGraph, Graph } from '@tuprolog/arg2p';
import type { GraphData } from '../engine.js';

// The graphs of @tuprolog/arg2p as the components read them: of a solver of the page (see host), or of the goals of a
// parallel solver (ThreadSolver.goals). The solver itself is not loaded with it.

/** A graph of the API as plain data, read in [profile] */
export function graphData(graph: Graph, profile = ''): GraphData {
    return {
        profile,
        arguments: graph.labellings.map(({ argument, label }) => ({
            id: argument.identifier,
            descriptor: argument.descriptor,
            conclusion: argument.conclusion,
            topRule: argument.topRule,
            label,
            supports: argument.supports.map(it => it.identifier),
        })),
        attacks: graph.attacks.map(it => ({
            from: it.attacker.identifier,
            to: it.target.identifier,
            type: it.type ?? null,
            on: it.on?.identifier ?? null,
        })),
    };
}

/** A compact graph of the API as the same plain data, read in [profile]: no object of the API for an argument */
export function compactGraphData(graph: CompactGraph, profile = ''): GraphData {
    const { texts, conclusions, topRules, attackers, targets, on, attackTypeOf, attackTypes, supporters, supported } = graph;
    const ids = Array.from({ length: graph.size }, (_, position) => graph.identifier(position));
    const supports: string[][] = ids.map(() => []);
    supported.forEach((argument, i) => supports[argument].push(ids[supporters[i]]));
    return {
        profile,
        arguments: Array.from(graph.order, position => ({
            id: ids[position],
            descriptor: graph.descriptor(position),
            conclusion: texts[conclusions[position]],
            topRule: texts[topRules[position]],
            label: graph.label(position),
            supports: supports[position],
        })),
        attacks: Array.from(attackers, (attacker, i) => ({
            from: ids[attacker],
            to: ids[targets[i]],
            type: attackTypes[attackTypeOf[i]],
            on: ids[on[i]],
        })),
    };
}
