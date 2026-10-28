---
title: Abstract Evaluation
weight: 26
---

## Abstract Evaluation

The **abstract evaluation** feature allows you to compute extensions and labellings of an **abstract argumentation framework** (AF) using various semantics.  

An **argumentation framework** is defined by:
- A set of **arguments**  
- A set of **attacks** between arguments  

The solver provides two main entry points:

---

### `abstract::solve/5`

```prolog
abstract::solve(Arguments, Attacks, I, O, U).
````

* **First parameter (`Arguments`)**: list of arguments
* **Second parameter (`Attacks`)**: list of attacks (pairs of arguments)
* **Third parameter (`I`)**: the set of arguments labelled **In**
* **Fourth parameter (`O`)**: the set of arguments labelled **Out**
* **Fifth parameter (`U`)**: the set of arguments labelled **Undecided**

This version returns the computed labelling under the chosen semantics.

#### Usage Example

```prolog
?- abstract::solve([a, b, c], [(a, b), (b, c)], I, O, U).

I = [c, a],
O = [b],
U = [].
```

In this example, under **grounded** semantics:

* Arguments `a` and `c` are **In**
* Argument `b` is **Out**
* No argument is **Undecided**

---

### `abstract::solve/2`

```prolog
abstract::solve(Arguments, Attacks).
```

This version **does not return values directly**.
Instead, the **argumentation graph** can be inspected programmatically or using:

* The **Graph tab** in the Java IDE
* The **Web IDE graph visualization**

---

## Semantics Support

All standard semantics are supported. The evaluation mode can be configured via the `argumentLabellingMode/1` flag. See the [Flags Reference]({{% ref "/docs/api/flags" %}}) for the full list of accepted values.

## Drawing a framework instead of writing one

In the [Web Playground](https://tuprolog.github.io/arg2p-kt-web/), a framework is a theory under the `abstract`
[profile]({{% ref "/docs/profiles" %}}) (`graphBuildMode(abstract).`, arguments `a.`, attacks `a -> b.` and
`a <-> b.`), and it is drawn: add arguments and attacks on the canvas and the theory is written for you; pick a
semantics in the flags, solve, and the drawing is recoloured with the resulting labelling. Hovering or tapping an
argument tells its label, what attacks it and what it attacks. Frameworks are imported from APX, TGF and ICCMA'23
files, exported to APX and TGF, and shared as links:
[try the framework above](https://tuprolog.github.io/arg2p-kt-web/?arguments=a%2Cb%2Cc&attacks=a-b%2Cb-c),
built from `?arguments=a,b,c&attacks=a-b,b-c`. Both `,` and `;` separate items, and an attack
may be written `a-b` or `a>b`; arguments mentioned only in `attacks` are added to the graph automatically.
