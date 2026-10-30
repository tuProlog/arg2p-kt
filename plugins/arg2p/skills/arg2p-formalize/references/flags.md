# The flags of Arg2P

Flags are Prolog facts. A theory can state them; a solver is given them beside the theory. A flag the theory states
replaces the value given for it, and a profile (`graphBuildMode`) presets several. Full reference:
<https://tuprolog.github.io/arg2p-kt/docs/api/flags/>.

| Flag | Default | Values |
| --- | --- | --- |
| `graphBuildMode(P)` | `aspic_plus` | `aspic_plus`, `aspic`, `aspic_defeasible`, `meta`, `aba`, `aba_plus`, `abstract`, `free` |
| `argumentLabellingMode(S)` | `grounded` | `grounded`, `complete`, `preferred`, `stable`, `semistable`, `ideal`, `eager`, `naive`, `stage`, `cf2`, `stage2`, `conflictfree`, `admissible`, `stronglyadmissible`, `bp_grounded`, `bp_grounded_partial`, `bp_grounded_complete` |
| `statementLabellingMode(M)` | `statement` | `statement`, `statement_pass_through`, `statement_binary` |
| `graphExtension(E)` (may repeat) | `standardPref` (with `rebutRestriction` under `aspic_plus`; `rebutRestriction` alone under `aspic`) | `standardPref`, `defeasiblePref`, `defeasibleAllPref`, `rebutRestriction`, `bp`, `abaPref`, `constraints` (statements over regions of numbers: `from(T)`, `until(T)`, `between(A, B)`); `graphExtension(none).` states none |
| `orderingPrinciple(P)` | `last` | `last`, `weakest` |
| `orderingComparator(C)` | `elitist` | `elitist`, `democrat`, `normal` (with `last` only) |
| `conflictModel(M)` (may repeat) | all four parts (`declared` alone under `aba`, `aba_plus`) | `negation` (`a` vs `-a`), `deontic` (`o`/`p` pairs), `preference` (`sup(X, Y)` vs `sup(Y, X)`), `declared` (`conflict/2,3`); `builtin` for all four, `conflictModel(none).` for none (only the contraries of the languages) |
| `queryMode` | on with no flags given | present or absent |
| `autoTransposition` | on under `aspic_plus` and `aspic` | present, or `autoTransposition(off).` |
| `prologStrictCompatibility` | off | present: the Prolog clauses of the theory are argued |
| `metaRules`, `metaConflicts` | off (on under `meta`) | present: rules fire only when argued `applicable(Rule)`, conflicts only when argued `conflict(A, B)` |
| `temporal` | off | present: the axioms of time over `happens(E, T)`, `initiates(E, U)`, `terminates(E, U)`; concludes `holdsAt(U, from(T1))`, `holdsAt(U, until(T2))`, `holdsAt(U, between(T1, T2))`, and answers `holdsAt(U, T)` by the region containing T |
| `graphBuilder(B)` | `ruleGraph` | `ruleGraph`, `abstractGraph`: set by the profile |
| `graphLanguage(L)` | none | `aspicLanguage`, `abaLanguage`, `defeasibleLanguage`, `abstractLanguage`: set by the profile |
| `debug` | off | present: `bp_grounded_partial` writes the steps of its labelling |
| `naturalTerms` | off | present (in the theory only): natural-language-like terms are normalised as the rules are parsed |
| `modulesPath(P)` | `none` | a directory or a base URL `call_module/2` loads modules from |

## Semantics

- `grounded`: skeptical, exactly one labelling. "What must be accepted?"
- `preferred`, `stable`, `complete`, `semistable`, `stage`, `cf2`, `stage2`, `naive`: several labellings, each a
  solution of its own. A statement in every labelling is **skeptically** accepted; in at least one, **credulously**.
  `stable` may have none.
- `bp_grounded*`: required by a theory declaring `bp/N`.

## Statement labels

With `statement` (the default), a statement is in if some in argument concludes it, otherwise out if some out argument
does, otherwise und. `statement_binary` merges out and und.

## Ordering

`orderingPrinciple`: `last` compares the last defeasible rules of two arguments, `weakest` all of them (an argument is
as strong as its weakest link). `orderingComparator`: `elitist`, an argument is weaker if some of its rules is weaker
than all of the other's; `democrat`, if each of its rules is weaker than some of the other's; `normal`, the attack
relation of Dung and Thang on the last rules (facts written as rules, `r : [] => a.`).
