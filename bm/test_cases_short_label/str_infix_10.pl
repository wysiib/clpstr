:- use_module('../../src/clpstr').

benchmark(Options) :-
  str_in(X,".*"),
  str_infix(X,".........."),
  labeling(Options,[X]).
