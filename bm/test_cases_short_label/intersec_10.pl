:- use_module('../../src/clpstr').

benchmark(Options) :-
  str_in(X,".........."),
  str_in(Y,".........."),
  str_intersection(X,Y,Z),
  labeling(Options,[X,Y,Z]).
