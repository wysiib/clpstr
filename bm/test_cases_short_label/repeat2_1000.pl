:- use_module('../../src/clpstr').

benchmark(Options) :-
  str_in(X,".*"),
  str_repeat(X,1000,Y),
  str_labeling(Options,[X,Y]).
