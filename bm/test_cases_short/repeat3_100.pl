:- use_module('../../src/clpstr').

benchmark(_) :-
  str_in(X,".*"),
  str_repeat(X,1,100,_).
