%! locality(-Locality)
%  Query mode: the label of an argument only depends on the arguments it depends on (directionality)
locality(directional).

%! argumentLabelling
%  The grounded labelling, in the active context.
argumentLabelling :-
    groundedFixpoint.

%! propagateFrom(+Id)
%  In a context labelled with a grounded fixpoint, the UND argument Id labelled IN and the fixpoint extended
%  from it alone: the labels argumentLabelling would give after unlabelling UND and labelling Id IN.
