:- use_module('../../src/clpstr').

benchmark(Options) :-
  str_in(X,".*"),
  str_suffix(X,"...................................................................................................."),
  labeling(Options,[X]).
