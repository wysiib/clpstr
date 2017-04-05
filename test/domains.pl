:- use_module(library(plunit)).

:- use_module('../src/domains/basic_domains').

:- begin_tests(basic_domains).

test(constant_domain,[true(D == string_dom("abc"))]) :-
  constant_string_domain("abc",D).

:- end_tests(basic_domains).