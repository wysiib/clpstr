:- use_module('../src/clpstr').

% This code example checks for attacking code in an sql injection.

input_check(Input) :-
  string(Input),
  \+ check_for_attack_code(Input).

check_for_attack_code(Input) :-
  str_in(X,Input),
  str_infix(X,"\' OR \'1\' = \'1").
