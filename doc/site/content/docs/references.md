---
title: References
weight: 45
---

## References

Arg2P is a research artefact: most of what it implements is described in a paper. This page collects the
references, grouped by the part of the framework they underpin.

---

## The framework

The reference paper for Arg2P as a whole:

> Roberta Calegari, Andrea Omicini, Giuseppe Pisano, Giovanni Sartor.
> **Arg2P: an argumentation framework for explainable intelligent systems.**
> _Journal of Logic and Computation_, 32(2):369–401, 2022.
> [10.1093/logcom/exab089](https://doi.org/10.1093/logcom/exab089)

A broader treatment of the framework, its meta-models and the reasoning machinery behind it:

> Giuseppe Pisano.
> **Argumentation for legal reasoning: meta-models, technology and beyond.**
> PhD thesis, Alma Mater Studiorum — Università di Bologna, Law, Science and Technology, 36th cycle, 2024.
> [10.48676/unibo/amsdottorato/11671](https://doi.org/10.48676/unibo/amsdottorato/11671)

### Predecessors

The tool Arg2P grew out of:

> Giuseppe Pisano, Roberta Calegari, Andrea Omicini, Giovanni Sartor.
> **Arg-tuProlog: A tuProlog-based argumentation framework.**
> CILC 2021, CEUR Workshop Proceedings 2719:51–66, 2020.

> Roberta Calegari, Giuseppe Contissa, Giuseppe Pisano, Galileo Sartor, Giovanni Sartor.
> **Arg-tuProlog: A modular logic argumentation tool for PIL.**
> JURIX 2020, Frontiers in Artificial Intelligence and Applications, 265–268, 2020.

---

## Burden of persuasion

The model implemented by the `bp_grounded`, `bp_grounded_partial` and `bp_grounded_complete`
[semantics]({{% ref "/docs/flags" %}}):

> Roberta Calegari, Giovanni Sartor.
> **Burden of persuasion in argumentation.**
> ICLP 2020, Electronic Proceedings in Theoretical Computer Science 325:151–163, 2020.
> [10.4204/EPTCS.325.21](https://doi.org/10.4204/EPTCS.325.21)

> Roberta Calegari, Giovanni Sartor.
> **A model for the burden of persuasion in argumentation.**
> JURIX 2020, Frontiers in Artificial Intelligence and Applications 334:13–22, 2020.

> Roberta Calegari, Régis Riveret, Giovanni Sartor.
> **The burden of persuasion in structured argumentation.**
> ICAIL 2021, 180–184, 2021.

The meta-argumentation approach behind the `graphExtension(bp)` extension, where the burden is expressed
inside the rules themselves:

> Giuseppe Pisano, Roberta Calegari, Andrea Omicini, Giovanni Sartor.
> **Burden of persuasion in meta-argumentation.**
> AIxIA 2021, Lecture Notes in Computer Science, 104–119, 2022.
> [10.1007/978-3-031-08421-8_8](https://doi.org/10.1007/978-3-031-08421-8_8)

> Giuseppe Pisano, Roberta Calegari, Andrea Omicini, Giovanni Sartor.
> **Burden of persuasion: a meta-argumentation approach.**
> _Journal of Applied Logics_, 10(3):393–420, 2023.

---

## Preferences

The mechanism behind `graphExtension(defeasiblePref)` and `graphExtension(defeasibleAllPref)`:

> Giuseppe Pisano, Roberta Calegari, Andrea Omicini, Giovanni Sartor.
> **A mechanism for reasoning over defeasible preferences in Arg2P.**
> CILC 2021, CEUR Workshop Proceedings 3002:16–30, 2021.
> [ceur-ws.org/Vol-3002/paper10.pdf](https://ceur-ws.org/Vol-3002/paper10.pdf)

---

## Modularity

The modular argumentation model behind the [module system]({{% ref "/docs/modules" %}}) — theory
fragmentation, modules that coexist and interact, and module nesting:

> Roberta Calegari, Giuseppe Contissa, Giuseppe Pisano, Galileo Sartor, Giovanni Sartor.
> **Modular logic argumentation in Arg-tuProlog.**
> AIxIA 2021, Lecture Notes in Computer Science, 91–103, 2022.
> [10.1007/978-3-031-08421-8_7](https://doi.org/10.1007/978-3-031-08421-8_7)

---

## Conflicts

The treatment of conflicts that the parser and the `metaConflicts` flag build on:

> Giuseppe Pisano, Roberta Calegari, Henry Prakken, Giovanni Sartor.
> **Arguing about the existence of conflicts.**
> COMMA 2022, Frontiers in Artificial Intelligence and Applications 353:284–295, 2022.

---

## Causality

The model implemented by `ness_original/3` and `ness_original_intervention/2` in the
[causality module]({{% ref "/docs/modules/causality" %}}):

> Giuseppe Pisano, Henry Prakken, Giovanni Sartor, Ruta Liepina.
> **Modelling cause-in-fact in legal cases through defeasible argumentation.**
> ICAIL 2025, 278–287, 2025.
> [10.1145/3769126.3769228](https://doi.org/10.1145/3769126.3769228)

The revised model behind `ness/3` and `ness_intervention/2` is to appear at ICAIL 2026; this page will be
updated once it is published.

Related work on causal reasoning in law from the same group:

> Ruta Liepina, Giuseppe Pisano, Giovanni Sartor.
> **Addressing causal puzzles in law through argumentation.**
> JURIX 2024, Frontiers in Artificial Intelligence and Applications 395:381–383, 2024.

---

## Distributed reasoning

The cooperative, multi-agent evaluation behind the
[distributed solver]({{% ref "/docs/modules/actor-solver" %}}):

> Giuseppe Pisano, Roberta Calegari, Andrea Omicini.
> **Multi-agent cooperative argumentation in Arg2P.**
> AIxIA 2022, Lecture Notes in Computer Science, 140–153, 2023.
> [10.1007/978-3-031-27181-6_10](https://doi.org/10.1007/978-3-031-27181-6_10)

---

## Citing Arg2P

If you use Arg2P in academic work, please cite the journal paper:

```bibtex
@article{arg2p,
  author  = {Calegari, Roberta and Omicini, Andrea and Pisano, Giuseppe and Sartor, Giovanni},
  title   = {{Arg2P}: an argumentation framework for explainable intelligent systems},
  journal = {Journal of Logic and Computation},
  volume  = {32},
  number  = {2},
  pages   = {369--401},
  year    = {2022},
  doi     = {10.1093/logcom/exab089}
}
```
