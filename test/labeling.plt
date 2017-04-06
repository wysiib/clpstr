:- use_module(library(plunit)).

:- use_module('../src/domains/basic_domains').
:- use_module('../src/labeling').

:- begin_tests(basic_domains).

test(constant,[true(Res == "abc")]) :-
  constant_string_domain("abc",D),
  label(D,Res).

test(simple_automaton,[true(Res == "a")]) :-
  label(automaton_dom([start,end],[(start,range(97,97),end)],[start],[end]),Res).

test(any_char_domain,[true(Res == " "),nondet]) :-
  any_char_domain(D),
  label(D,Res).

:- end_tests(basic_domains).
