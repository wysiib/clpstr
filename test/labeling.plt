:- use_module(library(plunit)).

:- use_module('../src/domains/basic_domains').
:- use_module('../src/labeling')

:- begin_tests(basic_domains).

test(constant,[true(Res == "abc")]) :-
  constant_string_domain("abc",D),
  labeling(D,Res).

test(any_char_domain,[true(D == " "),nondet]) :-
  any_char_domain(D),
  labeling(D,Res).
  
:- end_tests(basic_domains).
