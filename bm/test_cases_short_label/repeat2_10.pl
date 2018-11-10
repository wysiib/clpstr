:- use_module('../../src/clpstr').

benchmark(Options) :-
  str_in(X,".*"),
  str_repeat(X,10,Y),
  str_labeling(Options,[X,Y]).
