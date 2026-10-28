# The language of Arg2P

Every statement is a named rule or a fact, ending with a full stop. Rule names are unique atoms. Premises and
conclusions are Prolog terms (atoms, compound terms, lists); variables (uppercase) make rules schematic. Full
reference: <https://tuprolog.github.io/arg2p-kt/docs/syntax/>.

## Rules and facts

```prolog
r1 : premise1, premise2 => conclusion.   % defeasible rule
r2 : premise1 -> conclusion.             % strict rule
r3 : [] => conclusion.                   % defeasible rule with no premise
f1 :=> claim.                            % ordinary premise: can be attacked
a1 :-> fact.                             % axiom: cannot be attacked
```

**An axiom cannot be attacked**: rebutting and undermining only apply to arguments with a defeasible part.

```prolog
f0 :=> p.       % ordinary premise
a1 :-> p.       % axiom: p is in, -p is out
f1 :=> -p.
```

## Negation and exceptions

```prolog
-flies(X)                              % strong negation: conflicts with flies(X); cannot be nested
r : ~(exception), condition => c.      % weak negation (in a body): unless exception is established, if condition then c
r1 : other => undercut(r0).            % r0 does not apply
r1 : other => -r0.                     % the same; -r0 can itself be attacked, preferred, derived
```

## Preferences

```prolog
sup(r1, r2).    % r1 is more reliable than r2
```

Preferences are transitive. Without `sup/2`, two conflicting defeasible conclusions are both undecided. With the
default ordering (`orderingPrinciple(last)`, `orderingComparator(elitist)`), an argument with no defeasible rule or
premise is stronger than one with some, whatever the preferences.

Preferences can be concluded by rules (`r : condition => sup(r1, r2).`): add `graphExtension(defeasiblePref).` (or
`defeasibleAllPref` for comparators needing several superiorities) to the flags.

## Custom conflicts

```prolog
conflict([a], [b]).            % an argument for a attacks one for b (one way)
conflict([b], [a]).            % state both for a mutual conflict, as a and -a are
conflict([a], [b], Guard).     % ... whenever the Prolog goal Guard succeeds
```

## Obligations and permissions

```prolog
o(Term)       % obligation              o(-Term)   % prohibition
-o(Term)      % no obligation           -o(-Term)  % no prohibition
p(Term)       % permission              p(-Term)   % permission not to
v : o(-enter), enter => violation.
```

## Calling Prolog

```prolog
r : age(X), prolog(X >= 18) => adult(X).
```

A `prolog/1` premise is a computation, never attacked. Write it **after** the premises binding its variables
(`prolog(X >= 18), age(X)` raises an instantiation error). `min/2` and `max/2` are not available inside it: use
if-then-else. Recursive counting or summing rules make argument construction stall after a few levels: prefer
closed-form indices and one flat sum.

## Burden of persuasion

```prolog
bp(guilty).
```

Read by the semantics `bp_grounded`, `bp_grounded_partial`, `bp_grounded_complete`: a statement carrying the burden
that cannot be established is out, instead of undecided. A burden open to argument is concluded by a rule instead
(`r : [] => bp(guilty).`), with `graphExtension(bp).`.

## Other frameworks

**Abstract** (`graphBuildMode(abstract).`): `a.` an argument, `a -> b.` an attack, `a <-> b.` a mutual one; Prolog
clauses may derive them (`X -> Y :- beats(X, Y).`).

**ABA** (`graphBuildMode(aba).`): assumptions, their contraries, strict rules.

```prolog
graphBuildMode(aba).

assumption(a).
assumption(b).
contrary(a, x).     % an argument for x attacks every argument resting on a
r1 : b -> x.
```

`aba_plus` adds preferences between assumptions, `sup(Stronger, Weaker)`. Both refuse non-flat theories (a rule
concluding an assumption).

**Meta-argumentation** (`graphBuildMode(meta).`): a rule fires only when an argument concludes `applicable(Rule)`, two
statements conflict only when an argument concludes `conflict(A, B)` (even `a` and `-a`), and the preferences are the
`sup/2` arguments conclude. A theory written for `aspic_plus` is not one for `meta`: its premises stand, but its rules
never fire and its conflicts are none.

## Worked patterns

Each of these is solved by Arg2P as its comment says (grounded semantics).

**A conflict resolved by a preference.**

```prolog
r1 : [] => d.
r2 : [] => -d.
sup(r1, r2).     % d in, -d out; without sup/2 both und
```

**A default with an exception.**

```prolog
r1 : bird(X), ~(penguin(X)) => flies(X).
r2 : penguin(X) -> bird(X).
f1 :=> penguin(tweety).     % flies(tweety) is not concluded
```

**A deadlock broken by the burden of persuasion** (with `argumentLabellingMode(bp_grounded).`).

```prolog
bp(guilty).
r1 : evidence => guilty.
r2 : alibi => -guilty.
f1 :=> evidence.
f2 :=> alibi.     % guilty out, instead of und
```
