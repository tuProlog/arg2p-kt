---
---

## Defeasible Rules
All statements are expressed in this form:

    RuleName: Premise1, ..., PremiseN => Conclusion.


The absence of premises, necessary for facts codification, can be expressed with the notation:

    FactName: [] => Conclusion.


As for the form that premises and conclusions can take, all the properties of prolog terms (atoms, variables, lists, compound terms) are allowed.


## Strong negation

    -Term

indicates a strong negation, as opposed to the negation as failure implemented within the tuProlog engine. Strong negation cannot be nested.

## Weak negation

    ~(Term)

X indicates a weak negation -- i.e., negation by failure -- as the ability to encode rules exceptions. 
Weak negations are admitted only inside the body of the rule (premises). Accordingly, the rule:

    r : ~(Term1), Term2 => Conclusions.

should be read as _unless Term1, if Term2 then Conclusions_.


## Permission and obligation

    p(Term)
    o(Term)

to indicate permission and obligation respectively. These concepts, belonging to the deontic expansion of classical logic, allow obtaining the flexibility necessary to deal with prohibitions and rights. For instance:

    v_rule: o(-enter), enter => violation.

Currently admitted forms for permission and obligation:

    o(Term)     obligation
    o(-Term)    prohibition
    -o(Term)    no obligation
    -o(-Term)   no prohibition
    p(Term)     permission to do something
    p(-Term)    permission to don't do something

where _Term_ is a standard Prolog term.

## Superiority Relation

It is possible to express these preferences with the following notation:


    sup(RuleNam1, RuleName2).


This proposition symbolises the greater reliability of the rule with identifier equal to _RuleName 1_ over that with identifier _RuleName 2_.

## Burden of proof

The indication of burden of persuasion can be expressed as:

    bp(Term1,…, TermN).

