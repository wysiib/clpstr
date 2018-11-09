:- use_module('../../src/clpstr').

benchmark(Options) :-
  str_in(X,".*"),
  str_prefix(X,"...................................................................................................."),
  labeling(Options,[X]).
