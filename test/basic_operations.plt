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

:- begin_tests(mixed_domains).

test(simple_mixed_one_char,[true(D == string_dom("a"))]) :-
  constant_string_domain("a",D1),
  any_char_domain(D2),
  intersection(D1,D2,D).
test(simple_mixed_one_char_reverse,[true(D == string_dom("a"))]) :-
  constant_string_domain("a",D1),
  any_char_domain(D2),
  intersection(D2,D1,D).  
test(simple_mixed_card,[true(D == empty)]) :-
  constant_string_domain("ab",D1),
  any_char_domain(D2),
  intersection(D1,D2,D).
test(simple_mixed_card_reverse,[true(D == empty)]) :-
  constant_string_domain("ab",D1),
  any_char_domain(D2),
  intersection(D2,D1,D).

:- end_tests(mixed_domains).

:- begin_tests(empty_domains).

test(is_empty) :-
  constant_string_domain("abc",D1),
  constant_string_domain("def",D2),
  intersection(D1,D2,D),
  is_empty(D).

:- end_tests(empty_domains).
