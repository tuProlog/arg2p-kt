%! locality(-Locality)
%  Query mode: the label of an argument only depends on the arguments it depends on (directionality)
locality(directional).

%! argumentLabelling
%  Each stage2 labelling in turn, left as the active context.
argumentLabelling :-
    cf2::compute(stage).

