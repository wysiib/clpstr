:- use_module('../../src/clpstr').

benchmark(Options) :-
  str_in(X,".*"),
  str_size(X,100),
  labeling(Options,[X]).
