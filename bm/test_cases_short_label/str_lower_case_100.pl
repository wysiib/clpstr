:- use_module('../../src/clpstr').

benchmark(Options) :-
  str_in(X,"...................................................................................................."),
  str_lower_case(X),
  str_labeling(Options,[X]).
