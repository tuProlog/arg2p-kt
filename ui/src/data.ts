// What the components are built on, with no component: the contract of the engines, the flags and the profiles, the
// formats of abstract frameworks. It needs no DOM (@tuprolog/arg2p-ui/data).
export * from './engine.js';
export { parseFramework, toApx, toTgf, toTheory, frameworkOf, type Framework, type ReadFramework } from './formats/frameworks.js';
export { declaredFramework } from './formats/declarations.js';
export {
    FLAG_GROUPS,
    FLAGS,
    PROFILES,
    DEFAULTS,
    DEFAULT_FLAGS,
    DEFAULT_PROFILE,
    SEMANTICS,
    profileInfo,
    type Flag,
    type FlagGroup,
    type FlagOption,
    type ProfileInfo,
    type Settings,
} from './flags/schema.js';
export { statedFlags, statedProfile, profileOf, withValues, withNone, withSwitch, without } from './flags/text.js';
export type { Positions } from './graph/positions.js';

// How the graph and the drawing draw (the layout of the rows of a graph, the names of its arguments, a theory rewritten
// by drawing): kept for the pages that took them, they are internal, no part of the API, and may change in any version
export { layeredLayout, type Link, type Point, type Size, type LayoutOptions } from './graph/layout.js';
export { levelsOf, supportsOf } from './graph/levels.js';
export { isAbstract, nameOf } from './graph/drawing.js';
// What links an argument to the statement of the theory it comes from (a page links statements to what they are about)
export { statementOf, sourceStatement, isLinked, passagesOf } from './graph/drawing.js';
export { redrawn } from './formats/declarations.js';
