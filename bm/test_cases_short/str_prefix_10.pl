:- use_module('../../src/clpstr').

benchmark(_) :-
  str_in(X,".*"),
  str_prefix(X,"..........").
