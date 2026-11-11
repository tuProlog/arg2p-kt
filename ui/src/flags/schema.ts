// The flags of arg2p (see the Flags page of its documentation) and its profiles, the values of graphBuildMode that
// preset them (see the Profiles page). The values are those of the enumerations of the API (Semantics, GraphExtension,
// ...): the tests of the host check them against it.

/** A value a flag may take: what it is called, and what it does */
export interface FlagOption {
    readonly value: string;
    readonly label: string;
    readonly help: string;
}

/**
 * A flag: `choice` has one value, `set` several (the fact may appear several times), `switch` is on or off (the fact
 * is there or not), `text` has a free value. A [required] flag needs a value for the solver to run. The [aliases] of a
 * set are values standing for several of its options (the `builtin` of conflictModel, all its parts): a text stating
 * one is read as stating those, and the panel writes the options alone.
 */
export interface Flag {
    readonly name: string;
    readonly kind: 'choice' | 'set' | 'switch' | 'text';
    readonly label: string;
    readonly help: string;
    readonly options?: readonly FlagOption[];
    readonly aliases?: Readonly<Record<string, readonly string[]>>;
    readonly required?: boolean;
    readonly placeholder?: string;
}

export interface FlagGroup {
    readonly id: 'semantics' | 'preferences' | 'evaluation' | 'framework';
    readonly title: string;
    readonly flags: readonly Flag[];
}

/** A profile: what it reads a theory as, and the groups of flags that mean something for it */
export interface ProfileInfo {
    readonly name: string;
    readonly label: string;
    readonly help: string;
    readonly groups: readonly FlagGroup['id'][];
}

/** The values of the flags in effect for a theory, by flag: a switch on has the value `true` */
export type Settings = Readonly<Record<string, readonly string[]>>;

const option = (value: string, label: string, help: string): FlagOption => ({ value, label, help });

/** The parts of the conflict model (conflictModel), all of them by default: `builtin` stands for them all */
export const CONFLICT_PARTS: readonly string[] = ['negation', 'deontic', 'preference', 'declared'];

export const SEMANTICS: readonly FlagOption[] = [
    option('grounded', 'Grounded', 'The least complete labelling: what is beyond doubt.'),
    option('complete', 'Complete', "Dung's complete labellings."),
    option('conflictFree', 'Conflict-free', 'Labellings with no attack between in arguments.'),
    option('admissible', 'Admissible', 'Conflict-free labellings that defend their in arguments.'),
    option('stronglyAdmissible', 'Strongly admissible', 'Admissible labellings built up from unattacked arguments.'),
    option('preferred', 'Preferred', 'The maximal admissible labellings.'),
    option('semiStable', 'Semi-stable', 'Complete labellings with the fewest und arguments.'),
    option('stable', 'Stable', 'Labellings with no und argument.'),
    option('ideal', 'Ideal', 'The largest admissible labelling within every preferred one.'),
    option('eager', 'Eager', 'The largest admissible labelling within every semi-stable one.'),
    option('naive', 'Naive', 'The maximal conflict-free labellings.'),
    option('stage', 'Stage', 'Conflict-free labellings with the fewest und arguments.'),
    option('cf2', 'CF2', 'Naive labellings of the strongly connected parts, in turn.'),
    option('stage2', 'Stage2', 'Stage labellings of the strongly connected parts, in turn.'),
    option('bpGrounded', 'Burden of persuasion (grounded)', 'The grounded labelling with burdens of persuasion.'),
    option('bpGroundedPartial', 'Burden of persuasion (partial)', 'The partial labelling with burdens of persuasion.'),
    option('bpGroundedComplete', 'Burden of persuasion (complete)', 'The complete labelling with burdens of persuasion.'),
];

export const FLAG_GROUPS: readonly FlagGroup[] = [
    {
        id: 'semantics',
        title: 'Semantics',
        flags: [
            {
                name: 'argumentLabellingMode',
                kind: 'choice',
                required: true,
                label: 'Argument semantics',
                options: SEMANTICS,
                help: 'Labels the arguments in, out or und. Semantics with several labellings give one solution each: use Next.',
            },
            {
                name: 'statementLabellingMode',
                kind: 'choice',
                required: true,
                label: 'Statement semantics',
                options: [
                    option('statement', 'Standard', 'In if some in argument concludes it, otherwise out if some out argument does, otherwise und.'),
                    option('statementPassThrough', 'Pass-through', 'Passes the labels of the arguments on as they are.'),
                    option('statementBinary', 'Binary', 'In if some in argument concludes it, out otherwise.'),
                ],
                help: 'How the labels of the arguments pass to the statements they conclude.',
            },
        ],
    },
    {
        id: 'preferences',
        title: 'Preferences',
        flags: [
            {
                name: 'graphExtension',
                kind: 'set',
                label: 'Graph extensions',
                options: [
                    option('standardPref', 'Static preferences', 'Attacks that do not succeed as defeats are removed by the ordering of the arguments.'),
                    option('defeasiblePref', 'Defeasible preferences', 'A sup/2 concluded by a rule counts as a preference (single superiorities).'),
                    option('defeasibleAllPref', 'All defeasible preferences', 'Comparisons that need several superiorities at once, too.'),
                    option('rebutRestriction', 'Restricted rebut', 'An argument cannot rebut a conclusion drawn by a strict rule.'),
                    option('abaPref', 'ABA+ preferences', 'An attack from an assumption weaker than the one it attacks is reversed (sup/2).'),
                    option('bp', 'Arguable burden of persuasion', 'The burden is concluded by a rule, as r : [] => bp(x).'),
                ],
                help: 'Applied to the graph once it is built. None ticked: no preference.',
            },
            {
                name: 'orderingPrinciple',
                kind: 'choice',
                required: true,
                label: 'Ordering principle',
                options: [
                    option('last', 'Last link', 'Compares the last defeasible rules of the arguments.'),
                    option('weakest', 'Weakest link', 'Compares all their defeasible rules: an argument is as strong as its weakest.'),
                ],
                help: 'Which rules of two arguments are compared.',
            },
            {
                name: 'orderingComparator',
                kind: 'choice',
                required: true,
                label: 'Ordering comparator',
                options: [
                    option('elitist', 'Elitist', 'Weaker if some of its rules is weaker than all the rules of the other.'),
                    option('democrat', 'Democrat', 'Weaker if each of its rules is weaker than some rule of the other.'),
                    option('normal', 'Normal', 'The attack relation of Dung and Thang, on the last rules (with the last link alone).'),
                ],
                help: 'How the rules picked by the principle are compared.',
            },
        ],
    },
    {
        id: 'evaluation',
        title: 'Evaluation',
        flags: [
            {
                name: 'answerMode',
                kind: 'choice',
                required: true,
                label: 'Answer mode',
                options: [
                    option('enumerate', 'Each labelling', 'An answer for each labelling.'),
                    option('credulous', 'Credulous', 'One answer: in if in some labelling, out if out in every one.'),
                    option('skeptical', 'Skeptical', 'One answer: in if in every labelling, out if out in some.'),
                ],
                help: 'How a query is answered with a semantics of several labellings.',
            },
            { name: 'queryMode', kind: 'switch', label: 'Query mode', help: 'Builds only the part of the graph the query needs: much faster on large theories.' },
            { name: 'autoTransposition', kind: 'switch', label: 'Transposition', help: 'Closes the strict rules under transposition, as the rationality postulates need.' },
            { name: 'prologStrictCompatibility', kind: 'switch', label: 'Prolog clauses argued', help: 'The Prolog clauses of the theory are argued too.' },
            { name: 'metaConflicts', kind: 'switch', label: 'Meta conflicts', help: 'Conflicts are arguments as well, which may be attacked.' },
            { name: 'metaRules', kind: 'switch', label: 'Meta rules', help: 'Rules are arguments as well, which may be attacked.' },
            {
                name: 'modulesPath',
                kind: 'text',
                label: 'Modules path',
                placeholder: 'https://example.org/theories',
                help: 'Where callModule/2 loads modules from, a directory or a base URL. Empty: none.',
            },
        ],
    },
    {
        id: 'framework',
        title: 'Framework',
        flags: [
            {
                name: 'graphBuilder',
                kind: 'choice',
                label: 'Graph builder',
                options: [
                    option('ruleGraph', 'Rules', 'The arguments built from the rules of the theory.'),
                    option('abstractGraph', 'Arguments and attacks', 'The arguments and attacks the theory states.'),
                ],
                help: 'How the graph is built. The profile sets it.',
            },
            {
                name: 'graphLanguage',
                kind: 'set',
                label: 'Languages',
                options: [
                    option('aspicLanguage', 'ASPIC+', 'Contraries well formed.'),
                    option('abaLanguage', 'ABA', 'Assumptions and contraries, flat, strict rules alone.'),
                    option('defeasibleLanguage', 'Defeasible', 'Defeasible rules alone.'),
                    option('abstractLanguage', 'Abstract', 'Arguments and attacks alone, none of the notations of rules.'),
                ],
                help: 'What the theory is read and checked as, beyond the language of arg2p. The profile sets them.',
            },
            {
                name: 'conflictModel',
                kind: 'set',
                label: 'Conflicts',
                options: [
                    option('negation', 'Negation', 'A literal and its negation: a and -a.'),
                    option('deontic', 'Deontic', 'Obligations and permissions: o(a) and o(-a), p(a) and o(-a), o(a) and p(-a), o(a) and -p(a).'),
                    option('preference', 'Preference', 'A superiority and its converse: sup(r1, r2) and sup(r2, r1).'),
                    option('declared', 'Declared', 'The conflicts the theory declares, conflict/2 and conflict/3 (alone, as the contraries of ABA).'),
                ],
                aliases: { builtin: CONFLICT_PARTS },
                help: 'The conflicts beyond those of the languages of the theory, as the contraries of ABA, which always hold. None ticked: those alone.',
            },
        ],
    },
];

export const FLAGS: readonly Flag[] = FLAG_GROUPS.flatMap(group => group.flags);

const ALL_GROUPS: readonly FlagGroup['id'][] = ['semantics', 'preferences', 'evaluation', 'framework'];

export const PROFILES: readonly ProfileInfo[] = [
    { name: 'aspicPlus', label: 'ASPIC+', help: 'Strict and defeasible rules, axioms and premises, preferences, restricted rebut, transposition.', groups: ALL_GROUPS },
    { name: 'aspic', label: 'ASPIC+ without preferences', help: 'ASPIC+ where every attack succeeds.', groups: ALL_GROUPS },
    { name: 'aspicDefeasible', label: 'ASPIC+, defeasible rules', help: 'Defeasible rules alone (facts as r : [] => a.), preferences by the last link.', groups: ALL_GROUPS },
    { name: 'meta', label: 'ASPIC+ with meta-argumentation', help: 'ASPIC+ reasoning about its own rules, conflicts and preferences.', groups: ALL_GROUPS },
    { name: 'aba', label: 'ABA', help: 'Flat assumption-based argumentation: strict rules, assumption/1, contrary/2.', groups: ['semantics', 'evaluation', 'framework'] },
    { name: 'abaPlus', label: 'ABA+', help: 'Flat ABA with preferences between assumptions (sup/2), attacks from weaker ones reversed.', groups: ALL_GROUPS },
    { name: 'abstract', label: 'Abstract framework', help: 'Dung frameworks: arguments (a.), attacks (a -> b.) and mutual attacks (a <-> b.).', groups: ['semantics'] },
    { name: 'free', label: 'Free', help: 'No preset: every flag as the flags and the theory state it.', groups: ALL_GROUPS },
];

export const DEFAULT_PROFILE = 'aspicPlus';

/**
 * The values of the flags as arg2p spelled them before 0.26.0 (and the papers on it), by that spelling: read as those of
 * 0.26.0, as arg2p reads them (EarlierSpellings in the core)
 */
export const EARLIER_VALUES: Readonly<Record<string, string>> = {
    aspic_plus: 'aspicPlus',
    aspic_defeasible: 'aspicDefeasible',
    aba_plus: 'abaPlus',
    standard_af: 'aspicPlus',
    conflictfree: 'conflictFree',
    stronglyadmissible: 'stronglyAdmissible',
    semistable: 'semiStable',
    grounded_hash: 'grounded',
    bp_grounded: 'bpGrounded',
    bp_grounded_partial: 'bpGroundedPartial',
    bp_grounded_complete: 'bpGroundedComplete',
    statement_pass_through: 'statementPassThrough',
    statement_binary: 'statementBinary',
};

/**
 * The value of every flag when nothing states it (those of Flags.defaults(), a switch on as `true`): the flags of the
 * solver override what the profile presets only with other values than these
 */
export const DEFAULTS: Settings = {
    graphBuildMode: [DEFAULT_PROFILE],
    argumentLabellingMode: ['grounded'],
    statementLabellingMode: ['statement'],
    graphExtension: ['standardPref'],
    orderingPrinciple: ['last'],
    orderingComparator: ['elitist'],
    answerMode: ['enumerate'],
    queryMode: ['true'],
    autoTransposition: [],
    prologStrictCompatibility: [],
    metaConflicts: [],
    metaRules: [],
    modulesPath: [],
    graphBuilder: ['ruleGraph'],
    graphLanguage: [],
    conflictModel: CONFLICT_PARTS,
};

/**
 * The flags a new theory starts with: its semantics, and query mode (a switch is off unless stated). The solver fills
 * in the others with the values of the profile, or their defaults: stated, they would keep the profile from presetting
 * them only where they differ from the defaults.
 */
export const DEFAULT_FLAGS = ['argumentLabellingMode(grounded).', 'statementLabellingMode(statement).', 'queryMode.', ''].join('\n');

export function profileInfo(name: string): ProfileInfo | undefined {
    return PROFILES.find(it => it.name === name);
}
