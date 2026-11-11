---
title: Abstract Evaluation
weight: 26
---

## Abstract frameworks

An **abstract argumentation framework** (AF) is a set of **arguments** and of **attacks** between them. In arg2p it is a
theory of the `abstract` [profile]({{% ref "/docs/profiles" %}}), solved as any other theory:

```prolog
graphBuildMode(abstract).

a.
b.
c.
a -> b.
b -> c.
```

An argument is stated as `a.` or `arg(a).`, an attack as `a -> b.`, `att(a, b).`, or `a <-> b.` for a mutual one; both may be
derived by Prolog clauses (`X -> Y :- beats(X, Y).`), and an argument attacking or attacked is an argument as well. Any
other clause is data for these.

Each argument concludes its own name, so the statements of the framework are its arguments:

```prolog
?- solve(I, O, U).

I = [[c], [a]],
O = [[b]],
U = [].

?- solve(b, I, O, U).

I = [],
O = [b],
U = [].
```

Under **grounded** semantics `a` and `c` are **In**, `b` is **Out**. The graph of the framework can be inspected as any
other: in the [playground](https://tuprolog.github.io/arg2p-kt-web/), in the extension of VS Code, or through the API.

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
[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=f1%20%3A%3D%3E%20a.%0Af2%20%3A%3D%3E%20-a.&query=arg2p%3A%3Asolve&flags=statementLabellingMode%28statement%29.%0AargumentLabellingMode%28preferred%29.%0AorderingPrinciple%28last%29.%0AorderingComparator%28elitist%29.%0AgraphExtension%28standardPref%29.%0AqueryMode.)
— the link carries the whole flag block, since a shared link replaces the flags editor wholesale.
