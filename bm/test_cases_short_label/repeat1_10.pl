:- use_module('../../src/clpstr').

benchmark(Options) :-
  str_in(X,".........."),
  str_repeat(X,Y),
  labeling(Options,[X,Y]).
