:- use_module('../../src/clpstr').

benchmark(Options) :-
  str_in(X,".........."),
  labeling(Options,[X]).
