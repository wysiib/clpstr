:- use_module(library(plunit)).

:- use_module('../src/domains/basic_domains').

:- begin_tests(basic_domains).

test(constant_domain,[true(D == string_dom("abc"))]) :-
  constant_string_domain("abc",D).

test(any_char_domain,[true(D == automaton_dom([start,end],[(start,range(32,126),end)],[start],[end]))]) :-
  any_char_domain(D).
  
:- end_tests(basic_domains).
