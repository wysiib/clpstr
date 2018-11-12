:- use_module('../../src/clpstr').

benchmark(Options) :-
  str_in(X,".*"),
  str_infix(X,"...................................................................................................."),
  str_labeling(Options,[X]).
