:- use_module(library(plunit)).

:- use_module('../src/domains/basic_domains').
:- use_module('../src/domains/basic_operations').

:- begin_tests(constant_domains).

test(constant_domain_intersection_same,[true(D == D1)]) :-
  constant_string_domain("abc",D1),
  intersection(D1,D1,D).
test(constant_domain_intersection_different,[true(D == empty)]) :-
  constant_string_domain("abc",D1),
  constant_string_domain("def",D2),
  intersection(D1,D2,D).


:- end_tests(constant_domains).

:- begin_tests(empty_domains).

test(is_empty) :-
  constant_string_domain("abc",D1),
  constant_string_domain("def",D2),
  intersection(D1,D2,D),
  is_empty(D).



:- end_tests(empty_domains).
