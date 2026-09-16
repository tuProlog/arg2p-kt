---
title: Abstract Evaluation
weight: 35 
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

I = [a,c],
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

All standard semantics are supported. The evaluation mode can be configured via the `argumentLabellingMode\1` flag. See the [Flags Reference]({{% ref "/docs/flags" %}}) for the full list of accepted values.

## Drawing a framework instead of writing one

The [Web Playground](https://tuprolog.github.io/arg2p-kt-web/) has an **Abstract** mode that builds these
queries for you: add arguments and attacks on the canvas, pick a semantics, and it runs
`abstract::solve(Arguments, Attacks, In, Out, Und)` and recolours the drawing with the resulting labelling.
Frameworks can be shared as a link:
[try the framework above](https://tuprolog.github.io/arg2p-kt-web/?mode=abstract&arguments=a%2Cb%2Cc&attacks=a-b%2Cb-c),
built from `?mode=abstract&arguments=a,b,c&attacks=a-b,b-c`. Both `,` and `;` separate items, and an attack
may be written `a-b` or `a>b`; arguments mentioned only in `attacks` are added to the graph automatically.
