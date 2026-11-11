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

// What links an argument to the statement of the theory it comes from: the passage a page shows for a statement
export { sourceStatement } from './graph/drawing.js';
