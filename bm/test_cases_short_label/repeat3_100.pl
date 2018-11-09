:- use_module('../../src/clpstr').

benchmark(Options) :-
  str_in(X,".*"),
  str_repeat(X,1,100,Y),
  labeling(Options,[X,Y]).
