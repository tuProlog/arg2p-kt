%! rules(-Rules)
%  The clauses named in a comment, as defeasible rules.
rules(Rules) :- prologDefeasibleRules(Read), Rules = Read.

%! strictRules(+Rules, +Taken, -Added)
%  The clauses with no name, as strict rules.
strictRules(_, _, Added) :- prologStrictRules(Read), Added = Read.

%! premises(-Premises)
%  The facts named in a comment, as ordinary premises.
premises(Premises) :- prologPremises(Read), Premises = Read.

%! axioms(-Axioms)
%  The facts with no name, as axioms.
axioms(Axioms) :- prologAxioms(Read), Axioms = Read.
