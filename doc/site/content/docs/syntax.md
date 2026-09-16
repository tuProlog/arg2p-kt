---
title: Language
weight: 20
---

## Rules

The Arg2p framework allows the encoding of both strict and defeasible rules.

### Defeasible Rules

All statements are expressed in this form:

```prolog
RuleName: Premise1, ..., PremiseN => Conclusion.
```

The absence of premises can be expressed with the notation:

```prolog
RuleName: [] => Conclusion.
```

It is also possible to encode defeasible/ordinary premises with the notation:

```prolog
FactName :=> Conclusion.
```

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=r1%20%3A%20premise%20%3D%3E%20conclusion.%0Af1%20%3A%3D%3E%20premise.&query=arg2p%3A%3Asolve%28conclusion%2C%20Res%29)
— a rule and the premise that triggers it.

As for the form that premises and conclusions can take, all the properties of prolog terms (atoms, variables, lists, compound terms) are allowed.

### Strict Rules

All statements are expressed in this form:

```prolog
RuleName: Premise1, ..., PremiseN -> Conclusion.
```

The absence of premises can be expressed with the notation:

```prolog
RuleName: [] -> Conclusion.
```

It is also possible to encode axioms premises with the notation:

```prolog
FactName :-> Conclusion.
```

### Axioms or ordinary premises?

The distinction matters: **an axiom cannot be attacked**. Rebutting and undermining only apply to arguments
with a defeasible component, and an axiom has none, so a conflicting argument cannot touch it.

```prolog
f0 :=> p.       % ordinary premise: p and -p are both undecided
a1 :-> p.       % axiom: p holds, -p is out
f1 :=> -p.
```

Use `:->` for what the theory takes for granted — statutory text, agreed facts, definitions — and `:=>` for
what a counter-argument should be able to challenge.

[Try the axiom](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=a1%20%3A-%3E%20p.%0Af1%20%3A%3D%3E%20-p.&query=arg2p%3A%3Asolve%28p%2C%20Res%29)
and compare it with
[the ordinary premise](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=f0%20%3A%3D%3E%20p.%0Af1%20%3A%3D%3E%20-p.&query=arg2p%3A%3Asolve%28p%2C%20Res%29).

As for the form that premises and conclusions can take, all the properties of prolog terms (atoms, variables, lists, compound terms) are allowed.

## Conflicts

The contrast between terms, at the base of rebuts and undermine attacks, can be reached through negation.

### Strong negation

```prolog
-Term
```

indicates a strong negation, as opposed to the negation as failure implemented within the tuProlog engine. Strong negation cannot be nested.

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=f1%20%3A%3D%3E%20d.%0Af2%20%3A%3D%3E%20-d.&query=arg2p%3A%3Asolve%28d%2C%20Res%29)
— two premises supporting `d` and `-d` leave both undecided.

### Weak negation

```prolog
~(Term)
```

X indicates a weak negation -- i.e., negation by failure -- as the ability to encode rules exceptions. 
Weak negations are admitted only inside the body of the rule (premises). Accordingly, the rule:

```prolog
r : ~(Term1), Term2 => Conclusions.
```

should be read as _unless Term1, if Term2 then Conclusions_.

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=r1%20%3A%20bird%28X%29%2C%20%7E%28penguin%28X%29%29%20%3D%3E%20flies%28X%29.%0Af1%20%3A%3D%3E%20bird%28tweety%29.%0Af2%20%3A%3D%3E%20penguin%28tweety%29.&query=arg2p%3A%3Asolve%28flies%28tweety%29%2C%20Res%29)
— knowing Tweety is a penguin blocks the rule.

### Undercut

Undercut attacks can be expressed through the notation:

```prolog
undercut(ruleName)
```

where `ruleName` is the identifier of a defeasible rule in the theory.
For example, we could write:

```prolog
r0 : something => conclusion.
r1 : some_other_thing => undercut(r0).
```

Negating the rule name has the same effect, since a rule name conflicts with its own negation:

```prolog
r1 : some_other_thing => -r0.
```

Both forms make `r0` inapplicable; pick whichever reads better. The explicit `undercut/1` says what is meant,
while `-r0` composes with everything else that works on conclusions — it can itself be attacked, preferred, or
derived by a chain of rules.

Try
[the explicit form](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=r0%20%3A%20something%20%3D%3E%20conclusion.%0Ar1%20%3A%20other%20%3D%3E%20undercut%28r0%29.%0Af1%20%3A%3D%3E%20something.%0Af2%20%3A%3D%3E%20other.&query=arg2p%3A%3Asolve%28conclusion%2C%20Res%29)
and
[the negated rule name](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=r0%20%3A%20something%20%3D%3E%20conclusion.%0Ar1%20%3A%20other%20%3D%3E%20-r0.%0Af1%20%3A%3D%3E%20something.%0Af2%20%3A%3D%3E%20other.&query=arg2p%3A%3Asolve%28conclusion%2C%20Res%29)
— both leave `conclusion` out.

## Permission and obligation

```prolog
p(Term)
o(Term)
```

to indicate permission and obligation respectively. These concepts, belonging to the deontic expansion of classical logic, allow obtaining the flexibility necessary to deal with prohibitions and rights. For instance:

```prolog
v_rule: o(-enter), enter => violation.
```

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=f1%20%3A%3D%3E%20o%28-enter%29.%0Af2%20%3A%3D%3E%20enter.%0Av_rule%20%3A%20o%28-enter%29%2C%20enter%20%3D%3E%20violation.&query=arg2p%3A%3Asolve%28violation%2C%20Res%29)

Currently, admitted forms for permission and obligation are:

```prolog
o(Term)     % obligation
o(-Term)    % prohibition
-o(Term)    % no obligation
-o(-Term)   % no prohibition
p(Term)     % permission to do something
p(-Term)    % permission to don't do something
-p(Term)    % prohibition
-p(-Term)   % prohibition
```

where `Term` is a standard Prolog term.

## Superiority Relation

It is possible to express these preferences with the following notation:

```prolog
sup(RuleNam1, RuleName2).
```

This proposition symbolises the greater reliability of the rule with identifier equal to _RuleName 1_ over that with identifier _RuleName 2_.

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=r1%20%3A%20p%20%3D%3E%20q.%0Ar2%20%3A%20s%20%3D%3E%20-q.%0Af1%20%3A%3D%3E%20p.%0Af2%20%3A%3D%3E%20s.%0Asup%28r1%2C%20r2%29.&query=arg2p%3A%3Asolve%28q%2C%20Res%29)
— without `sup/2` both conclusions are undecided; with it, `q` wins.

## Custom conflicts

Beyond the built-in conflicts — a statement against its strong negation, and the deontic pairs — a theory can
declare its own:

```prolog
conflict([a], [b]).                 % a and b are incompatible
conflict([a], [b], Guard).          % ... whenever the Prolog goal Guard succeeds
```

Both arguments are **lists**, mirroring the way conclusions are stored. With the fact above, an argument for
`a` and one for `b` attack each other, exactly as `a` and `-a` would.

```prolog
conflict([a], [b]).
f1 :=> a.
f2 :=> b.
```

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=conflict%28%5Ba%5D%2C%20%5Bb%5D%29.%0Af1%20%3A%3D%3E%20a.%0Af2%20%3A%3D%3E%20b.&query=arg2p%3A%3Asolve%28b%2C%20Res%29)
— querying `b` shows the conflict at work; remove the `conflict/2` fact and it is accepted again.

To let rules *derive* conflicts instead of declaring them, see
[Meta-argumentation]({{% ref "/docs/modules/meta" %}}).

## Calling Prolog from a rule

A premise wrapped in `prolog/1` is executed as an ordinary Prolog goal instead of being treated as a statement
to be argued for:

```prolog
r1 : prolog(2 < 3) => ok.
```

The rule applies only if the goal succeeds, and the goal itself is never attacked — it is a computation, not a
claim. [Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=r1%20%3A%20prolog%282%20%3C%203%29%20%3D%3E%20ok.&query=arg2p%3A%3Asolve%28ok%2C%20Res%29)

This is the way to use arithmetic, comparisons, or any other Prolog builtin inside a theory:

```prolog
r_adult : age(X), prolog(X >= 18) => adult.
f1 :=> age(20).
```

If the goal fails, no argument is built from that rule at all.

> [!WARNING]
> A `prolog/1` goal whose variables are still unbound when the rule is considered — like `X` above, which only
> gets its value from the `age/1` premise — is solved while the whole graph is built, but **not** under
> `queryMode`, which `FlagsBuilder` enables by default. Ground goals such as `prolog(2 < 3)` work in both
> modes. Turn `queryMode` off for theories that compute with `prolog/1`.

## Burden of proof

The burden of persuasion indication can be expressed as:

```prolog
bp(Term1,…, TermN).
```

Declared this way it is read by the `bp_grounded` family of semantics.
[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=bp%28guilty%29.%0Ar1%20%3A%20evidence%20%3D%3E%20guilty.%0Ar2%20%3A%20alibi%20%3D%3E%20-guilty.%0Af1%20%3A%3D%3E%20evidence.%0Af2%20%3A%3D%3E%20alibi.&query=arg2p%3A%3Asolve%28guilty%2C%20Res%29&flags=graphBuildMode%28standard_af%29.%0AstatementLabellingMode%28statement%29.%0AargumentLabellingMode%28bp_grounded%29.%0AorderingPrinciple%28last%29.%0AorderingComparator%28elitist%29.%0AgraphExtension%28standardPref%29.%0AqueryMode.)
— the link carries the `bp_grounded` flag block.

A burden that should itself be open to argument is written as a rule conclusion instead — see
[Meta-argumentation]({{% ref "/docs/modules/meta" %}}).