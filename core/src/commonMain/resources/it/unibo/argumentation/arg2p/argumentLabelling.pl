% ----------------------------------------------------------------
% argumentLabelling.pl
% PIKA-lab
% Year: 2019
% ---------------------------------------------------------------


argumentLabelling([Arguments, _, _], [IN, OUT, UND]) :-
    labelArguments(Arguments, [], [], IN, OUT, UND),
    printArgumentLabelling( [IN, OUT, UND] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

labelArguments(UND, IN, OUT, ResultIN, ResultOUT, ResultUND) :-

   findall(A, ( member(A, UND), allAttacksOUT(A, OUT) ), NewListIN),
   append(IN, NewListIN, NewIN),
   findall(A, ( member(A, UND), oneAttackIN(A, NewIN) ), NewListOUT),
   append(OUT, NewListOUT, NewOUT),

   append(NewListIN, NewListOUT, NewLabelledArguments),
   \+ isEmptyList(NewLabelledArguments),

   subtract(UND, NewLabelledArguments, UndPlus1),
   labelArguments(UndPlus1, NewIN, NewOUT, ResultIN, ResultOUT, ResultUND).

labelArguments(UND, IN, OUT, IN, OUT, UND).

/*
    If an attack exists, it should come from an OUT argument
*/
allAttacksOUT(A, OUT) :-
    \+ ( attack(B, A),
    \+ ( member(B, OUT))
       ).

/*
    Find an attack, if exists, from an IN argument, then ends
*/
oneAttackIN(A, IN) :-
    attack(B, A),
    member(B, IN), !.