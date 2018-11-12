:- use_module('../../src/clpstr').

benchmark(Options) :-
  str_in(X,"...................................................................................................."),
  str_labeling(Options,[X]).
