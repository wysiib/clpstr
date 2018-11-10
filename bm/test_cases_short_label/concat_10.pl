:- use_module('../../src/clpstr').

benchmark(Options) :-
  str_in(X,".........."),
  str_in(Y,".........."),
  str_concatenation(X,Y,Z),
  str_labeling(Options,[X,Y,Z]).
