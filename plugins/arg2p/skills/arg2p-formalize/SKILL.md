---
name: arg2p-formalize
description: Formalize a reasoning problem with conflicting information, exceptions, norms, preferences or burdens of proof as an Arg2P argumentation theory, before solving it with arg2p-solve.
license: MIT
metadata:
  project: https://github.com/tuProlog/arg2p-kt
---

# Formalizing problems in Arg2P

[Arg2P](https://tuprolog.github.io/arg2p-kt/) is a structured argumentation engine (ASPIC<sup>+</sup>, ABA, abstract
frameworks) on the tuProlog engine. This skill turns a reasoning problem in natural language into an Arg2P **theory**
and the **flags** to solve it with. Computing the answer is the job of the `arg2p-solve` skill: never simulate the
semantics by hand.

## Step 1: choose the framework (the profile)

The profile is the first line of the theory, `graphBuildMode(Profile).`, and tells Arg2P which framework the theory is
written in. It refuses a theory written outside it, with an error saying why.

| Profile | Use it when | Refuses |
| --- | --- | --- |
| `aspic_plus` (the default) | knowledge is facts and rules, possibly conflicting, with exceptions, evidence, norms, preferences: **most real problems** | a contrary targeting an axiom or the conclusion of a strict rule |
| `aspic` | as `aspic_plus`, but no preference counts: every attack succeeds | as `aspic_plus` |
| `aspic_defeasible` | everything is defeasible, facts are rules with no premise (`r : [] => a.`) | strict rules, axioms, premises |
| `aba`, `aba_plus` | the problem is assumptions and their contraries (assumption-based argumentation); `aba_plus` adds preferences between assumptions | defeasible rules, ordinary premises, a rule concluding an assumption; for `aba_plus`, a theory without Weak Contraposition |
| `abstract` | only who attacks whom matters: a debate, a thread of objections, a graph given as such | rules, premises, axioms; preferences, conflicts, burdens (`sup/2`, `conflict/2`, `bp/N`) |
| `meta` | the rules, conflicts and preferences are themselves argued, as the readings of an ambiguous text are (step 3, and `references/syntax.md`) | |
| `free` | every flag is set by hand, with no preset | nothing |

What each profile presets and checks: <https://tuprolog.github.io/arg2p-kt/docs/profiles/>.

**Abstract** is right whenever the inner structure of the arguments does not matter. A debate can be abstracted on
purpose: each self-contained position is an argument, each "X objects to Y" an attack. Keep a glossary from each name
to the position it stands for.

```prolog
graphBuildMode(abstract).

a.            % an argument
b.
c.
a -> b.       % a attacks b
b <-> c.      % b and c attack each other
```

**Structured (ASPIC<sup>+</sup>)** is the default for real problems. If a structured encoding keeps collapsing (no
rules can be articulated, only pairwise disagreement), fall back to abstract.

## Step 2: extract the knowledge (structured theories)

1. List the atomic statements. Choose short Prolog terms (lowercase atoms, uppercase variables) and keep a **glossary**
   from each term to its meaning in natural language: the results are reported with it.
2. **Match facts against rule conditions verbatim before encoding.** For every rule premise you mean to satisfy with a
   given fact, quote the fact and the condition side by side and check they describe the same action or property of
   the same subjects. Problems are often seeded with near misses: "X falls on a square of Y" does not satisfy "X
   reveals a secret to Y"; "X has 5 friends" does not satisfy "X has more than 5 friends". Give distinct wordings
   distinct predicates. A condition no stated or derivable fact matches stays underivable: never invent a premise for
   it. An underivable premise yields a safe "undecided"; an invented one yields a confidently wrong answer.
3. Classify each piece of knowledge:
   - certain, unattackable fact: **axiom** `a1 :-> fact.`
   - claimed or observed fact that could be challenged: **ordinary premise** `f1 :=> fact.`
   - exceptionless inference (definitions, "all X are Y", mathematics): **strict rule** `r : body -> head.`
   - rule of thumb, presumption, "typically", "unless overridden": **defeasible rule** `r : body => head.`
4. Identify the conflicts: explicit negation `-`, exceptions (weak negation `~` or undercut), incompatible conclusions
   (`conflict/2`).
5. Identify the priorities: which rule or source wins when they clash, `sup/2`.
6. Identify the normative content: obligations `o/1`, permissions `p/1`.
7. Identify the burdens of proof: "X must be proven, otherwise rejected", `bp/N`.

Use axioms and strict rules only for what is truly indisputable: what rests on them cannot be attacked, so
counter-arguments become impossible. When in doubt, use the defeasible forms.

## Step 3: the interpretation ledger (any non-trivial source text)

A formalization inherits every misreading of its source. Before encoding a rulebook, statute, policy, or any text
longer than a few rules, write a ledger of the points where the text underdetermines the meaning:

- **Ordinal or optimal assignment**: "the first N items are free": by position, or to the claimant's best advantage?
- **Exception scope**: does an exception override one rule, a section, the whole policy? Does "unless" attach to the
  nearest clause or the whole sentence?
- **Default direction**: when a condition is not mentioned, does the rule still apply (defeasible) or not (strict
  precondition)?
- **Quantifier scope**: "a student who X": any one, or all?
- **Ties**: two applicable rules with no stated priority: the cheapest, the most specific, the first listed?

For each entry, record the readings, the one chosen and why. **When a reading changes the answer, encode both in
the theory**, with the profile `meta` ("Readings of an ambiguous clause" in `references/syntax.md`): each reading
argues the rules it applies, and the readings exclude each other. Solved under `preferred`, each labelling is one
reading; under `grounded`, what depends on the reading is und. A verdict that differs between the readings is
interpretation-bound: say so. The reading chosen is argued preferred (`sup/2`), and the other stays in the graph. If
the person who gave the text is reachable, ask them: the entry is already phrased as the question.

## Step 4: write the theory

The syntax, with examples of every construct, is in `references/syntax.md`. In short:

```prolog
r1 : bird(X), ~(penguin(X)) => flies(X).   % defeasible, unless penguin(X) is established
r2 : penguin(X) -> bird(X).                % strict
f1 :=> penguin(tweety).                    % ordinary premise
a1 :-> sparrow(jack).                      % axiom
r3 : sparrow(X) -> bird(X).
r4 : witness_unreliable => undercut(r1).   % r1 does not apply
sup(r1, r4).                               % r1 is preferred to r4
conflict([a], [b]).                        % a attacks b (one way: add conflict([b], [a]) for both)
r5 : age(X), prolog(X >= 18) => adult(X).  % a Prolog computation, after its variables are bound
bp(guilty).                                % the burden of persuasion is on guilty
```

Rule names are unique atoms. Without `sup/2`, two conflicting defeasible conclusions both end up undecided.

## Step 5: choose the flags

Flags are Prolog facts, given to the solver beside the theory (or written in the theory). With no flags, the solver
uses `argumentLabellingMode(grounded).`, `statementLabellingMode(statement).` and `queryMode.`; flags you give
**replace** those three, and what they leave out takes the value of the profile (query mode is then off unless you
state `queryMode.`). Every flag is in `references/flags.md`; the choices that matter:

- **Semantics**, `argumentLabellingMode(S)`:
  - `grounded` (the default): skeptical, always exactly one labelling. "What must be accepted?"
  - `preferred`, `stable`, `complete`, `semistable`, `stage`, `cf2`, ...: several labellings. "Which positions are
    defensible?" `stable` may have none.
  - `bp_grounded`, `bp_grounded_partial`, `bp_grounded_complete`: **required** when the theory declares `bp/N`.
- **Preferences concluded by rules** (`sup/2` in the head of a rule): `graphExtension(defeasiblePref).` (one
  superiority) or `graphExtension(defeasibleAllPref).` (comparators needing several). A theory stating
  `graphExtension` replaces the profile's: restate `graphExtension(standardPref).` if you still want static
  preferences.
- **Ordering**: `orderingPrinciple(last).` (the default) or `weakest.`; `orderingComparator(elitist).` (the default),
  `democrat.` or `normal.`
- **`queryMode.`**: goal-directed evaluation: only the arguments relevant to the query are built, which is faster on
  large theories. State it when you give flags of your own.

## Step 6: deliver

Write the theory to a `.arg2p` file (or keep it in the conversation for a short one), and give:

1. **The profile** and why.
2. **The glossary**: every term and rule name, and its meaning.
3. **The flags**, as Prolog facts, and why.
4. **The query**: a statement to label (`guilty`, `flies(X)`), a value the theory computes (`answer(V)`), or the whole
   graph (`arg2p::solve(In, Out, Und)`, the default).
5. **The interpretation ledger**, with the readings encoded for material ambiguities.
6. **The fidelity check**, last: re-read every premise and axiom of the finished theory against the source and
   confirm it quotes a stated fact. Delete any you cannot point to.

Then solve it with the `arg2p-solve` skill. Do not predict the labelling yourself.
