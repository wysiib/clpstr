:- use_module('../../src/clpstr').

benchmark(Options) :-
  str_in(X,".*"),
  str_repeat(X,1,1000,Y),
  labeling(Options,[X,Y]).
