:- use_module('../../src/clpstr').

benchmark(Options) :-
  str_in(X,".*"),
  str_size(X,100),
  str_labeling(Options,[X]).
