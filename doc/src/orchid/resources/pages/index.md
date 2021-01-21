---
title: Home
---

# Arg2P

Quick links:

- [GitLab Repository](https://gitlab.com/pika-lab/argumentation/arg2p) (the one used by developers)
- [GitLab Repository Test Release](https://gitlab.com/pika-lab/argumentation/arg2p/-/releases) (test used by developers)

- [GitHub Repository](https://github.com/tuProlog/arg2p)
- Maven Repository (where all releases are hosted) _(cooming soon)_

## Intro

<!--- ![Arg2P Logo][logo] --->

<!--- [logo]: {{ 'assets/media/logo-Arg2P.png'|asset|scale(0.65)  }} --->

<p align="center">
  <img width="460" src={{ 'assets/media/logo-Arg2P.png'|asset|scale(0.65)  }}>
</p>



> <span style="color: blue">Arg2P is an argumentation engine built on the top of [tuProlog (2P) engine](http://pika-lab.gitlab.io/tuprolog/2p-in-kotlin/). Arg2P is a lightweight implementation of the ASPIC<sup>+</sup>-like system for structured  argumentation</span>. 

In a nutshell, <span style="color: blue">arguments are produced from a set of defeasible rules</span>, and attack relationships between arguments are captured by argumentation graphs. The arguments of such graphs are labelled by following a labelling semantics. This simple framework will suffice to illustrate our upcoming investigation into persuasive burdens within an argumentation setting. 

In addition, we use defeasible rule schemata to account for <span style="color: blue">deontic</span> reasoning, towards doctrine reification, as presented in _Riveret et al., 2019_.

Moreover, the model has been extended following the <span style="color: blue">burden of proof</span> model presented in _Calegari and Sartor, 2020_.

More information about the format of the knowledge base and rules can be found on the [basic syntax]({{ site.baseUrl }}/wiki/syntax).